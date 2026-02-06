FROM debian:bookworm-slim

ENV BOT_BINARY_URL="https://github.com/maaad/haskell-tg-gh-watcher/releases/download/v0.0.1/git-watcher-bot-linux-amd64.zip"

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
      unzip && \
    rm -rf /var/lib/apt/lists/*

RUN useradd -m -u 1000 bot

WORKDIR /data

RUN curl -L "$BOT_BINARY_URL" -o /tmp/git-watcher-bot.zip && \
    unzip /tmp/git-watcher-bot.zip -d /usr/local/bin && \
    rm /tmp/git-watcher-bot.zip && \
    if [ -f /usr/local/bin/git-watcher-bot-linux-amd64 ] && [ ! -f /usr/local/bin/git-watcher-bot ]; then \
      mv /usr/local/bin/git-watcher-bot-linux-amd64 /usr/local/bin/git-watcher-bot; \
    fi && \
    chmod +x /usr/local/bin/git-watcher-bot*

COPY .env.sample /data/.env.sample

USER bot

CMD ["git-watcher-bot"]

