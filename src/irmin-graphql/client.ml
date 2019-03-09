open Lwt.Infix

let uri: Uri.t option Irmin.Private.Conf.key =
  Irmin.Private.Conf.key
    ~docv:"URI"
    ~doc:"Location of the remote store."
    "uri" Irmin.Private.Conf.(some uri) None

module Make
    (Client : Cohttp_lwt.S.Client)
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
          "object", `String (Irmin.Type.to_string C.t x)
        ] in
        Graphql.execute_json t ~vars query ["data"; "add_object"] >|= function
        | Some (`String hash) -> Graphql.unwrap (Irmin.Type.of_string H.t hash)
        | _ -> raise (Graphql.unwrap (Graphql.invalid_response "Contents.add"))

      let merge: [`Read | `Write] t -> key option Irmin.Merge.t = fun t ->
        let query = {|
          mutation MergeObjects($a: ObjectHash, $b: ObjectHash, $old: ObjectHash) {
            merge_objects(a: $a, b: $b, old: $old)
          }
        |} in
        Irmin.(Merge.v (Type.option Key.t) (fun ~old a b ->
          old () >>= function
          | Ok old ->
            let old = match old with
              | Some (Some x) -> Some (Some x)
              | Some None -> Some None
              | None -> None
            in
            let opt = function None -> `Null | Some x -> `String (Irmin.Type.to_string H.t x) in
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
            (match Irmin.Type.of_string C.t s with
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
          "node", `String (Irmin.Type.to_string Val.t v)
        ] in
        Graphql.execute_json t ~vars query ["data"; "add_node"] >|= function
        | Some (`String hash) -> Graphql.unwrap (Irmin.Type.of_string H.t hash)
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
            (match Irmin.Type.of_string Val.t s with
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
          "commit", `String (Irmin.Type.to_string Val.t x)
        ] in
        Graphql.execute_json t ~vars query ["data"; "add_commit"] >|= function
        | Some (`String hash) -> Graphql.unwrap (Irmin.Type.of_string H.t hash)
        | _ -> raise (Graphql.unwrap (Graphql.invalid_response "Contents.add"))

      let find t hash =
        let query = {|
          query FindCommit($hash: CommitHash!) {
            commit(hash: $hash) {
              info
              hash
              parents
            }
          }
        |} in
        let vars = [
          "hash", `String (Irmin.Type.to_string H.t hash)
        ] in
        Graphql.execute_json t ~vars query ["data"; "commit"] >|= function
        | Some (`O _) -> failwith "UNIMPLEMENTED"
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
        | None   -> invalid_arg "Irmin_graphql client: No URI specified"
        | Some u -> u

      let v config =
        let uri = get_uri config in
        let headers = None in
        let ctx = None in
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


      let w = Watch.v ()

      let find t branch =
        let query = {|
          query FindBranch($branch: BranchName!) {
            branch(name: $name) {
              hash
            }
          }
        |} in
        let vars = [
          "name", `String (Irmin.Type.to_string B.t branch)
        ] in
        Graphql.execute_json t ~vars query ["data"; "branch"; "hash"] >|= function
        | Some (`String s) -> Some (Graphql.unwrap (Irmin.Type.of_string H.t s))
        | _ -> None

      let mem t branch =
        find t branch >|= function
        | Some _ -> true
        | None -> false

      let list _t = failwith "UNIMPLEMENTED"

      let remove _t _branch = failwith "UNIMPLEMENTED"

      let set _t _branch _hash = failwith "UNIMPLEMENTED"

      let test_and_set _t _branch ~test:_test ~set:_set = failwith "UNIMPLEMENTED"

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

module KV (Client: Cohttp_lwt.S.Client)(Contents: Irmin.Contents.S) =
  Make
    (Client)
    (Irmin.Metadata.None)
    (Contents)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
