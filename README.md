## git-watcher-bot

Yet another telegram bot written in Haskell that watches GitHub repositories (stars, watchers, open issues, pull requests, forks) and sends concise, aggregated notifications with exact deltas for each changed metric.

### Commands

The bot is a single-user tool: only the user whose ID is set in `ADMIN_USER_ID` can use it. All commands and messages from other users are ignored.

- `/start` — show a short instruction.
- `/watch_repo` — prompt to send a repository to add (accepts `owner/repo` or a full GitHub URL).
- `/my_repos` — show the watch list.
- `/unwatch_repo` — simplified removal flow (inline keyboard omitted).
- `/check` — run an immediate check over all repositories (same logic as the watchdog), without waiting for the next cycle.

### Metrics and notifications

GitWatcherBot tracks the following GitHub counters for each watched repository:

- **Stars**
- **Watchers**
- **Open issues** (GitHub issues **excluding** pull requests)
- **Pull requests**
- **Forks**

On each check (manual `/check` or background watchdog run) the bot:

- Compares the latest counters with the values stored in the local database.
- If at least one metric changed, sends **a single message per repository** that includes:
  - Previous and current values for every changed metric.
  - The signed delta (for example, `+2`, `-1`).
- Skips repositories where nothing changed, to avoid noise.

### Setup

1. Install a recent GHC and Cabal (see project-wide `cabal.project` and `package.yaml`).
2. In the `git-watcher-bot` directory create a `.env` file:

   ```env
   BOT_TOKEN=6556...your token from @BotFather...
   ADMIN_USER_ID=135687116
   # optional, but recommended for private repositories and higher rate limits
   GITHUB_TOKEN=ghp_xxx...your GitHub Personal Access Token...
   ```

   - `BOT_TOKEN` — Telegram bot token from `@BotFather`.
   - `ADMIN_USER_ID` — your numeric Telegram user id. The bot is single-admin; only this user can interact with it.
   - `GITHUB_TOKEN` — GitHub Personal Access Token with read access to repositories. Used for private repositories and to increase GitHub API rate limits.

3. Build and run the bot (from the project root):

```bash
stack build
stack run git-watcher-bot-exe
```

### Configuration knobs

The following operational limits are defined in code and can be adjusted in `Config.Watch`:

- `perRepositoryDelaySeconds` — delay between checks for individual repositories inside a single watchdog/`/check` pass.
- `watchdogSleepSeconds` — sleep duration between full watchdog cycles.
- `databaseFile` / `databaseBackupFile` — names of the primary SQLite database file and its backup.

These values are chosen to be safe defaults for a single small bot instance; if you change them, make sure to also revisit your hosting and GitHub API rate limits.