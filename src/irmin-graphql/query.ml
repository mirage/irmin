let get = {|
  query Get($branch: BranchName!, $key: Key!) {
    branch(name: $branch) {
      get(key: $key)
    }
  }
|}

let get_tree = {|
  query GetTree($branch: BranchName!, $key: Key!) {
    branch(name: $branch) {
      get_tree(key: $key) {
        key
        value
        metadata
      }
    }
  }
|}

let set = {|
  mutation Set($branch: BranchName!, $key: Key!, $value: Value!, $info: InfoInput) {
    set(branch: $branch, key: $key, value: $value, info: $info) {
      hash
    }
  }
|}

let update_tree = {|
  mutation UpdateTree($branch: BranchName!, $key: Key!, $tree: [TreeItem!]!, $info: InfoInput) {
    update_tree(branch: $branch, key: $key, tree: $tree, info: $info) {
      hash
    }
  }
|}

let set_tree = {|
  mutation SetTree($branch: BranchName!, $key: Key!, $tree: [TreeItem!]! , $info: InfoInput) {
    set_tree(branch: $branch, key: $key, tree: $tree, info: $info) {
      hash
    }
  }
|}

let remove = {|
  mutation Remove($branch: BranchName!, $key: Key!, $info: InfoInput) {
    remove(branch: $branch, key: $key, info: $info) {
      hash
    }
  }
|}

let merge = {|
  mutation Merge($branch: BranchName, $from: BranchName!, $info: InfoInput) {
      merge(branch: $branch, from: $from, info: $info) {
          hash
      }
  }
|}

let push = {|
  mutation Push($branch: BranchName, $remote: Remote!) {
    push(branch: $branch, remote: $remote)
  }
|}

let pull = {|
  mutation Pull($branch: BranchName, $remote: Remote!, $info: InfoInput) {
    pull(branch: $branch, remote: $remote, info: $info) {
      hash
    }
  }
|}

let clone = {|
  mutation Clone($branch: BranchName, $remote: Remote!) {
    clone(branch: $branch, remote: $remote) {
      hash
    }
  }
|}

let revert = {|
  mutation Revert($branch: BranchName, $commit: CommitHash!) {
    revert(branch: $branch, commit: $commit) {
      hash
    }
  }
|}

let lca = {|
  query($branch: BranchName!, $hash: CommitHash!) {
    branch(name: $branch) {
      lca(commit: $hash) {
        hash,
        info {
          message,
          author,
          date
        }
        parents {
          hash
        }
      }
    }
  }
|}

let branch_info = {|
  query BranchInfo($branch: BranchName!) {
      branch(name: $branch) {
        name,
        head {
          hash,
          info {
            message,
            author,
            date
          }
          parents {
            hash
          }
        }
      }
  }
|}

let commit_info = {|
  query CommitInfo($hash: CommitHash!) {
    commit(hash: $hash) {
      hash,
      info {
          message,
          author,
          date
      }
      parents {
          hash
      }
    }
|}

let branches = "query { branches }"

let get_all = {|
  query GetAll($branch: BranchName!, $key: Key!) {
      branch(name: $branch) {
          get_all(key: $key) {
              value
              metadata
          }
      }
  }
|}


let set_all = {|
  mutation SetAll($branch: BranchName, $key: Key!, $value: Value!, $metadata: Metadata, $info: InfoInput) {
      set_all(branch: $branch, key: $key, value: $value, metadata: $metadata, info: $info) {
          hash
      }
  }
|}


let list =  {|
  query List($branch: BranchName!, $step: Step!) {
      branch(name: $branch) {
          head {
              node {
                  get(step: $step) {
                      tree {
                          key,
                          value
                      }
                  }
              }
          }
      }
  }
|}


let all = [
  "get", get;
  "get_tree", get_tree;
  "set", set;
  "update_tree", update_tree;
  "set_tree", set_tree;
  "remove", remove;
  "merge", merge;
  "push", push;
  "pull", pull;
  "clone", clone;
  "revert", revert;
  "lca", lca;
  "branch_info", branch_info;
  "commit_info", commit_info;
  "branches", branches;
  "get_all", get_all;
  "set_all", set_all;
  "list", list;
]

let generate_json () =
  let obj = List.map (fun (k, v) ->
      k, `String v) all
  in Json.to_string (`O obj)



