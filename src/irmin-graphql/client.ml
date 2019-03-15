open Lwt.Infix

let uri: Uri.t option Irmin.Private.Conf.key =
  Irmin.Private.Conf.key
    ~docv:"URI"
    ~doc:"Location of the GraphQL server"
    "uri" Irmin.Private.Conf.(some uri) None

let config u =
  let open Irmin.Private in
  let cfg = Conf.empty in
  let cfg = Conf.add cfg uri (Some u) in
  cfg

module type CLIENT = sig
  include Cohttp_lwt.S.Client
  val ctx: unit -> ctx option
end

module Make
    (Client : CLIENT)
    (M: Irmin.Metadata.S)
    (C: Irmin.Contents.S)
    (K: Irmin.Path.S)
    (B: Irmin.Branch.S)
    (H: Irmin.Hash.S) =
struct
  module X = struct
    module Graphql = Helper.Make(Client)(B)(H)

    module Hash = H

    module Sync = Irmin.Private.Sync.None(H)(B)

    module Contents = struct
      module Metadata = M
      module Key = struct
        include Hash
        let digest v = Hash.digest (Irmin.Type.to_string C.t v)
      end
      module Val = C
      type 'a t = Graphql.t
      type key = Key.t
      type value = Val.t

      let add t x =
        let query = {|
          mutation AddObject($object: String!) {
            add_object(object: $object)
          }
        |} in
        let vars = [
          "object", `String (Base64.encode_exn (Irmin.Type.to_string C.t x))
        ] in
        Graphql.execute_json t ~vars query ["data"; "add_object"] >|= function
        | Some (`String hash) -> Graphql.unwrap ~prefix:"Contents.add" (Irmin.Type.of_string H.t hash)
        | _ -> raise (Graphql.unwrap(Graphql.invalid_response "Contents.add"))

      let merge: [`Read | `Write] t -> key option Irmin.Merge.t = fun t ->
        let query = {|
          mutation MergeObjects($a: ObjectHash, $b: ObjectHash, $old: ObjectHash) {
            merge_objects(a: $a, b: $b, old: $old)
          }
        |} in
        Irmin.(Merge.v (Type.option Key.t) (fun ~old a b ->
          old () >>= function
          | Ok old ->
            let opt = function None -> `String "" | Some x -> `String (Irmin.Type.to_string H.t x) in
            let a = opt a in
            let b = opt b in
            let old = match old with Some old -> opt old | None -> `Null in
            let vars = [
              "a", a;
              "b", b;
              "old", old;
            ] in
            (Graphql.execute_json t ~vars query ["data"; "merge_objects"] >|= function
            | Some (`String s) ->
                (match Irmin.Type.of_string H.t s with
                | Ok x -> Ok (Some x)
                | Error _ -> Ok None)
            | _ -> Ok None)
          | Error e -> Lwt.return_error e))


      let find t k =
        let query = {|
          query FindObject($hash: ObjectHash!) {
            find_object(hash: $hash)
          }
        |} in
        let vars = [
          "hash", `String (Irmin.Type.to_string H.t k)
        ] in
        Graphql.execute_json t ~vars query ["data"; "find_object"] >|= function
        | Some (`String s) ->
            (match Irmin.Type.of_string C.t (Base64.decode_exn s) with
            | Ok x -> Some x
            | Error _ -> None)
        | Some `Null -> None
        | _ -> None

      let mem t k =
        find t k >|= function
        | Some _ -> true
        | None -> false
    end

    module Node = Irmin.Private.Node.Store(Contents)(K)(M)(struct
      module Key = Hash
      module Val = Irmin.Private.Node.Make(H)(K)(M)
      type 'a t = Graphql.t
      type key = Key.t
      type value = Val.t

      let add t v =
        let query = {|
          mutation AddNode($node: String!) {
            add_node(node: $node)
          }
        |} in
        let vars = [
          "node", `String (Base64.encode_exn (Irmin.Type.to_string Val.t v))
        ] in
        Graphql.execute_json t ~vars query ["data"; "add_node"] >|= function
        | Some (`String hash) -> Graphql.unwrap ~prefix:"Node.add" (Irmin.Type.of_string Key.t hash)
        | _ -> raise (Graphql.unwrap (Graphql.invalid_response "Node.add"))

      let find t k =
        let query = {|
          query FindNode($hash: ObjectHash!) {
            find_node(hash: $hash)
          }
        |} in
        let vars = [
          "hash", `String (Irmin.Type.to_string H.t k)
        ] in
        Graphql.execute_json t ~vars query ["data"; "find_node"] >|= function
        | Some (`String s) ->
            (match Irmin.Type.of_string Val.t (Base64.decode_exn s) with
            | Ok x -> Some x
            | Error _ -> None)
        | Some `Null -> None
        | _ -> None

      let mem t hash =
        find t hash >|= function
        | Some _ -> true
        | None -> false
    end)

    module Commit = Irmin.Private.Commit.Store(Node)(struct
      module Key = Hash
      module Val = Irmin.Private.Commit.Make(Hash)
      type 'a t = Graphql.t
      type key = Key.t
      type value = Val.t

      let add t x =
        let query = {|
          mutation AddCommit($commit: String!) {
            add_commit(commit: $commit)
          }
        |} in
        let vars = [
          "commit", `String (Base64.encode_exn (Irmin.Type.to_string Val.t x))
        ] in
        Graphql.execute_json t ~vars query ["data"; "add_commit"] >|= function
        | Some (`String hash) -> Graphql.unwrap ~prefix:"Commit.add" (Irmin.Type.of_string H.t hash)
        | _ -> raise (Graphql.unwrap (Graphql.invalid_response "Contents.add"))

      let find t hash =
        let query = {|
          query FindCommit($hash: CommitHash!) {
            commit(hash: $hash) {
              hash
              info {
                author
                message
                date
              }
              tree {
                hash
              }
              parents
            }
          }
        |} in
        let vars = [
          "hash", `String (Irmin.Type.to_string H.t hash)
        ] in
        Graphql.execute_json t ~vars query ["data"; "commit"] >|= function
        | Some (`O _ as j) ->
            let f p x = Json.get_string_exn x |> Irmin.Type.of_string Hash.t |> Graphql.unwrap ~prefix:("Commit.find," ^ p) in
            let node = Json.find_exn j ["tree"; "hash"] |> f "node"  in
            let parents = match Json.find_exn j ["parents"] with `A x -> (List.map (f "parents") x)  | _ -> [] in
            (match Json.find_exn j ["info"] with
            | `O _ as info ->
                let author = Json.find_exn info ["author"] |> Json.get_string_exn in
                let message = Json.find_exn info ["message"] |> Json.get_string_exn in
                let date = Json.find_exn info ["date"] |> Json.get_string_exn |> Int64.of_string in
                let info = Irmin.Info.v ~author ~date message in
                Some (Val.v ~info ~node  ~parents)
            | _ -> None)
        | _ -> None

      let mem t hash =
        find t hash >|= function
        | Some _ -> true
        | None -> false
    end)

    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)

    module Repo = struct
      type t = Graphql.t

      let contents_t (t: t): 'a Contents.t = t
      let node_t (t: t): 'a Node.t = t, t
      let commit_t (t: t): 'a Commit.t = (node_t t), t
      let branch_t t = t

      let get_uri config =
        match Irmin.Private.Conf.get config uri with
        | None   -> Uri.of_string "http://localhost:8080/graphql"
        | Some u -> u

      let v config =
        let uri = get_uri config in
        let headers = None in
        let ctx = Client.ctx () in
        let branch = None in
        Lwt.return (Graphql.v ?headers ?ctx ?branch uri)

      let batch t f =
        let node = t, t in
        let commit = node, t in
        f t node commit
    end

    module Branch = struct
      module Key = B
      module Val = H
      module Watch = Irmin.Private.Watch.Make(Key)(Val)
      type t = Graphql.t
      type key = Key.t
      type value = Val.t
      type watch = Watch.watch

      (* TODO: use websockets to subscribe to branch on graphql server *)
      let w = Watch.v ()

      let find t branch =
        let query = {|
          query FindBranch($branch: BranchName!) {
            branch(name: $name) {
              name,
              head {
                hash
              }
            }
          }
        |} in
        let vars = [
          "name", `String (Irmin.Type.to_string B.t branch)
        ] in
        Graphql.execute_json t ~vars query ["data"; "branch"; "head"; "hash"] >|= function
        | Some (`String s) ->
            Some (Graphql.unwrap ~prefix:"Branch.find" (Irmin.Type.of_string H.t s))
        | _ -> None

      let mem t branch =
        find t branch >|= function
        | Some _ -> true
        | None -> false

      let list t =
        let query = {| query { branches } |} in
        Graphql.execute_json t query ["data"; "branches"] >|= function
        | Some (`A l) -> List.map (fun x -> Irmin.Type.of_string B.t (Json.get_string_exn x) |> Graphql.unwrap ~prefix:"Branch.list") l
        | _ -> []

      let remove t branch =
        let query = {|
          mutation RemoveBranch($branch: BranchName!) {
            remove_branch(branch: $branch)
          }
        |} in
        let branch' = Irmin.Type.to_string B.t branch in
        let vars = ["branch", `String branch'] in
        Graphql.execute_json t ~vars query ["data"; "remove_branch"] >>= function
        | Some (`Bool _) -> Watch.notify w branch None
        | _ -> Graphql.unwrap (Error (`Msg "unable to remove branch"))

      let set t branch hash =
        let query = {|
          mutation SetBranch($branch: BranchName!, $commit: CommitHash!) {
            set_branch(branch: $branch, commit: $commit)
          }
        |} in
        let branch' = Irmin.Type.to_string B.t branch in
        let commit = Irmin.Type.to_string H.t hash in
        let vars = [
          "branch", `String branch';
          "commit", `String commit;
        ] in
        Graphql.execute_json t ~vars query ["data"; "set_branch"] >>= function
        | Some (`Bool true) -> Watch.notify w branch (Some hash)
        | _ -> Graphql.unwrap (Error (`Msg "unable to set branch"))


      let test_and_set t branch ~test ~set =
        let query = {|
          mutation TestAndSetBranch($branch: BranchName!, $test: CommitHash, $set: CommitHash) {
            test_and_set_branch(branch: $branch, test: $test, set: $set)
          }
        |} in
        let branch' = Irmin.Type.to_string B.t branch in
        let test = match test with Some test -> `String (Irmin.Type.to_string H.t test) | None -> `Null in
        let set' = match set with Some set -> `String (Irmin.Type.to_string H.t set) | None -> `Null in
        let vars = [
          "branch", `String branch';
          "test", test;
          "set", set';
        ] in
        Graphql.execute_json t ~vars query ["data"; "test_and_set_branch"] >>= function
        | Some (`Bool true) -> Watch.notify w branch set >>= fun () -> Lwt.return_true
        | _ -> Lwt.return_false

      let watch _t ?init x =
        Watch.watch w ?init x

      let watch_key _t key ?init x =
        Watch.watch_key w key ?init x

      let unwatch _t x =
        Watch.unwatch w x
    end
  end

  include Irmin.Of_private (X)
end

module KV (Client: CLIENT)(Contents: Irmin.Contents.S) =
  Make
    (Client)
    (Irmin.Metadata.None)
    (Contents)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
