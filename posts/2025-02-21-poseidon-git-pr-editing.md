---
title: "Editing a pull request branch created from a fork"
author: "Clemens Schmid"
origin: https://blog.poseidon-adna.org/posts/archive_editing_git.html
---

At the time of writing, the [Poseidon community archive](https://github.com/poseidon-framework/community-archive) has 14 open pull requests -- most of which were opened by various community members to add new packages to the archive. What certainly is a pleasant development, because it indicates that the archive gets adopted, also comes with technical and administrative challenges. As an editor for the archive I recently had to step up my Git skills to address a particular issue I was facing.

Already multiple times I found myself in the situation that I needed to edit a submission pull request before merging. This arose for example, when a package author prepared a package almost perfectly, but I still wanted to apply some additional minor changes before merging. Or an author or reviewer had struggled with Git, manoeuvred themselves into a predicament, and needed my help to untangle the knot without [starting from scratch](https://xkcd.com/1597). So here is what I came up with to allow me to do that efficiently.

## Workflow

GitHub's documentation includes a helpful tutorial how to [commit changes to a pull request branch created from a fork](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/committing-changes-to-a-pull-request-branch-created-from-a-fork). It already covers the basic workflow how to edit a fork. The article highlights a number of conditions for this to be possible:

> You can only make commits on pull request branches that:
>
> - Are opened in a repository that you have push access to and that were created from a fork of that repository
> - Are on a user-owned fork
> - Have permission granted from the pull request creator
> - Don't have branch restrictions that will prevent you from committing

All of these are met in my case. But two additional challenges complicate the matter: i) the community-archive uses Git LFS for the large data files, and ii) I need to do this so frequently, that cloning every fork feels unnecessarily cumbersome. The following workflow considers this special situation.

### 1. Clone the fork repository {#sec-clone}

```bash
GIT_LFS_SKIP_SMUDGE=1 git clone git@github.com:USERNAME/FORK-OF-THE-REPOSITORY.git
```

Note that this workflow assumes that you have installed and configured Git LFS on your system. Cloning the repo with the `GIT_LFS_SKIP_SMUDGE` environment variable prevents downloading the LFS-tracked files despite Git LFS being enabled. This saves bandwidth and costs for us on GitHub.

### 2. (if necessary) Switch to the pull request branch

```bash
git switch PR-BRANCH
```

This is only necessary, if the PR branch is not the main/master branch.

### 3. (if necessary) Download individual LFS-tracked files

```bash
git lfs pull --include "PATH-TO-FILE"
```

To validate a package completely it can be necessary to also access the genotype data. But because we cloned above with `GIT_LFS_SKIP_SMUDGE=1`, this data is not in our clone now. Fortunately we can selectively download it. `PATH-TO-FILE` can also include wildcards.

### 4. Edit the files as desired

Remember to commit the changes.

### 5. Push the changes

This should work with `git push`. But yet again, Git LFS complicates things, raising the following error message:

```txt
error: Authentication error: Authentication required: You must have push access to verify locks
error: failed to push some refs to 'github.com:USERNAME/FORK-OF-THE-REPOSITORY.git'
```

This is caused by a limitation of GitHub's Git LFS implementation. A long thread here discusses the issue: [Authentication required : You must have push access to verify locks error.](https://github.com/git-lfs/git-lfs/issues/2291#issuecomment-305887405) Multiple solutions are suggested there. One reliable workaround is to [delete the git hook `.git/hooks/pre-push`](https://github.com/git-lfs/git-lfs/issues/2291#issuecomment-305887405).

```bash
rm .git/hooks/pre-push
git push
```

This resolved the issue for me -- specifically because I never had to edit any of the genotype data files when working on a PR fork. I don't know how this hack affects the handling of LFS-tracked files.

### 6. (optional) Moving to another fork in the same clone

If the changes in a fork A are already merged into the master branch of the main archive repository, then a little trick allows to switch to another fork B in the same clone.

```bash
git remote -v
git remote set-url origin git@github.com:poseidon-framework/community-archive.git
git switch master
git pull
git remote set-url origin git@github.com:USERNAME/FORK-OF-THE-NEXT-REPOSITORY.git
git pull
```

We set the remote URL to the main repository, switch to the master branch, and pull. The commits from A are already there, so we have a clean state again. From here we can set a new remote URL for a fork B and pull. This effectively saves us from creating a fresh clone (as described in @sec-clone).