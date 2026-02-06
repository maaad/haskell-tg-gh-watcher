# Build stage: use official Haskell image with glibc
FROM haskell:9.4 AS build

WORKDIR /build

# Copy definition files first so dependency build can be cached
COPY git-watcher-bot.cabal package.yaml stack.yaml README.md ./

# Prime cabal store with dependencies only (cached between builds)
RUN cabal update && \
    cabal build exe:git-watcher-bot-exe --only-dependencies

# Now copy the actual source and build the executable
COPY src ./src
COPY app ./app

RUN cabal build exe:git-watcher-bot-exe && \
    cabal install exe:git-watcher-bot-exe \
      --installdir=/usr/local/bin \
      --overwrite-policy=always && \
    strip /usr/local/bin/git-watcher-bot-exe

# Runtime stage: small Debian image, only binary + minimal deps
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends ca-certificates && \
    rm -rf /var/lib/apt/lists/*

RUN useradd -m -u 1000 bot

WORKDIR /data

COPY --from=build /usr/local/bin/git-watcher-bot-exe /usr/local/bin/git-watcher-bot
COPY .env.sample /data/.env.sample

USER bot

CMD ["git-watcher-bot"]

