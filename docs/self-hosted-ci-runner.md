# Optional Self-Hosted CI Runner

This repository supports two optional self-hosted runner setups:

- A **sandboxed runner pool** for `nix flake check` on a Linux host, started and
  stopped as a foreground process. See [Sandboxed Runner Pool](#sandboxed-runner-pool).
- A **Lima-backed runner** for macOS hosts. See [Local Setup](#local-setup).

Both register repository-scoped runners. If no matching self-hosted runner is
online and idle, workflows fall back to a hosted runner instead of queueing
indefinitely on `self-hosted`.

## How CI Chooses a Runner

The workflows in [`.github/workflows/nix-flake-check.yml`](../.github/workflows/nix-flake-check.yml)
and [`.github/workflows/generated-reports-update.yml`](../.github/workflows/generated-reports-update.yml)
start with a small `select-runner` job on `ubuntu-24.04`.

That job:

- checks whether a repository self-hosted runner with the configured custom label is online and idle,
- only enables self-hosted execution for trusted contexts,
- emits a dynamic `runs-on` value for the main job,
- falls back to a hosted runner when the admin token is missing or the probe fails.

The self-hosted runner label defaults to `aihc-lima`.

The actual execution job keeps the same job name regardless of where it runs, so
required checks such as `flake-check` continue to work.

`main` is merged through a merge queue, so `nix-flake-check.yml` and
`minimum-ghc.yml` also run on `merge_group` events. A merge group is a trusted
context: its entries were queued by someone with write access, so the probe may
place those runs on the self-hosted runner just like a `push` to `main`.

## Repository Configuration

The workflow probe first tries:

- `SELF_HOSTED_RUNNER_ADMIN_TOKEN`

If that secret is not set, it falls back to:

- `AUTOMATION_PR_TOKEN`

The token is used only by the workflow probe job. The default workflow
`GITHUB_TOKEN` cannot call the repository self-hosted runner admin endpoints.

For the workflow probe, the token must be able to read the repository runner
administration API. If you use a fine-grained token, grant repository
`Administration` permission with `read-only` access. Reusing
`AUTOMATION_PR_TOKEN` can also work if it has that permission and is accepted by
the runner API.

Optional repository variables:

- `CI_SELF_HOSTED_RUNNER_LABEL`: defaults to `aihc-lima`.
- `CI_NIX_CHECK_RUNNER_LABEL`: label used by `nix-flake-check.yml` only. Falls
  back to `CI_SELF_HOSTED_RUNNER_LABEL`, then `aihc-lima`. Set this to point the
  flake check at the sandboxed pool while other workflows keep using the Lima
  runner.
- `CI_HOSTED_FALLBACK_RUNNER`: defaults to `blacksmith-4vcpu-ubuntu-2404`.

If you want to fall back to a GitHub-hosted runner instead of Blacksmith, set:

```text
CI_HOSTED_FALLBACK_RUNNER=ubuntu-24.04
```

## Local Setup

This section covers the Lima-backed runner for macOS hosts.

Requirements on the host:

- `gh`
- `limactl`
- `python3`

`gh` must be authenticated with a token that can administer self-hosted runners
for this repository.

Start the runner:

```bash
scripts/start-lima-runner.sh
```

Stop the runner and de-register it from GitHub:

```bash
scripts/stop-lima-runner.sh
```

Stop and delete the Lima VM completely:

```bash
scripts/stop-lima-runner.sh --delete-instance
```

Useful overrides:

```bash
scripts/start-lima-runner.sh \
  --runner-label aihc-lima \
  --instance-name aihc-gh-runner-me \
  --runner-name aihc-gh-runner-me \
  --cpus 6 \
  --memory 12 \
  --disk 80
```

## Lima Operational Notes

- Fork PRs do not use the self-hosted runner path.
- A hard VM stop can still leave an offline runner registration behind.
- The start script deletes stale offline registrations with the same runner name before registering again.
- The stop script first tries a graceful `config.sh remove`, then force-deletes the runner registration if it still exists.
- Do not use plain `limactl stop` if you want immediate cleanup on GitHub; use `scripts/stop-lima-runner.sh` instead.

## Sandboxed Runner Pool

`scripts/run-nix-runner-pool.sh` runs a pool of ephemeral runners on a Linux
host without installing anything or creating any services. It is intended for a
machine that is also used for other things.

Requirements on the host:

- `gh`, authenticated with admin access to the repository
- `podman` (preferred, rootless) or `docker`
- `python3`
- a running `nix-daemon`

Start four runners:

```bash
scripts/run-nix-runner-pool.sh --count 4 --runner-label aihc-nix
```

Then set the repository variable `CI_NIX_CHECK_RUNNER_LABEL=aihc-nix` so the
flake check targets the pool.

Press Ctrl-C to stop. The script removes its containers and de-registers its
runners on exit.

### How It Works

Each pool slot runs one container from `scripts/nix-runner/`, holding a single
`--ephemeral` runner. The runner takes exactly one job, de-registers itself, and
the container is discarded; the slot then starts a fresh one. No state carries
between jobs.

The image contains no Nix. Instead each container gets:

- `/nix` bind-mounted read-only, providing the store and the Nix binaries
- `/nix/var/nix/daemon-socket` bind-mounted read-write, so the client can connect
- `/etc/nix` bind-mounted read-only, for client settings and substituters
- `NIX_REMOTE=daemon` and a `PATH` starting at
  `/nix/var/nix/profiles/default/bin`, both set in the image and repeated on the
  run command

`NIX_REMOTE=daemon` is what routes builds to the host daemon. A Nix client only
needs the socket plus read access to the store, because the daemon performs
every write. This keeps containers disposable while the store stays warm across
jobs.

The entrypoint verifies the connection with `nix store info` before registering
the runner, so a broken mount or an unreadable socket fails immediately instead
of part-way through a job.

Because the runner already provides Nix, `nix-flake-check.yml` passes
`nix-preinstalled: true` to the `setup-nix-ci` action on self-hosted runs, which
skips both the Nix installer and the R2 cache setup. Configure substituters and
the cache signing key on the host instead.

### Two Sandboxes, Not One

The container isolates the runner and the job script. It does **not** isolate
Nix builds: those execute in the host `nix-daemon`, outside the container. Nix
builds are isolated by Nix's own build sandbox instead, which gives each
derivation private namespaces and no network.

Two consequences:

- `--cpus` and `--memory` bound the job script only. To limit what builds do to
  the machine, set `max-jobs` and `cores` in `/etc/nix/nix.conf`, or adjust the
  `nix flake check --max-jobs` flag in the workflow.
- Anything in the daemon's `trusted-users` can disable the build sandbox, which
  would defeat the isolation entirely. The pool prints the current value at
  startup. Do not add the account running the pool to that list.

### Security Notes

- The runner is repository-scoped, so it can only ever run this repository's
  workflows.
- Labels route jobs; they do not restrict them. Anyone who can push a branch can
  target the pool from a new workflow file. Treat the host accordingly.
- `nix flake check` builds Nix expressions taken from the pull request branch,
  so job content is attacker-controlled by design. The Nix build sandbox, not
  the command, is the boundary.
- Fork pull requests never reach self-hosted runners.
- The account running the pool holds a token that can register runners. Keep it
  out of the containers; the script passes only short-lived registration tokens.

### Pool Operational Notes

- A container killed mid-job leaves an offline registration behind. The script
  removes stale registrations matching its name prefix on exit.
- The workflow's store garbage collection step runs when disk use reaches 80%.
  With several runners active this can slow a job down; it is safe, since Nix
  respects the roots of in-flight builds.
- The image is tagged with the runner version and reused across restarts. Use
  `--rebuild-image` to force a rebuild, or `--runner-version` to pin.
