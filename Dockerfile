FROM clojure:temurin-17-tools-deps-1.11.3.1456-bullseye-slim

# Install dependencies for tests
RUN apt-get update && \
    apt-get install -y curl gnupg wget unzip chromium && \
    curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
    apt-get install -y nodejs && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

ENV CHROME_BIN=/usr/bin/chromium
ENV CHROME_FLAGS="--no-sandbox --headless --disable-gpu --disable-dev-shm-usage"

# Install clj-kondo
RUN curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo && \
    chmod +x install-clj-kondo && \
    ./install-clj-kondo && \
    rm install-clj-kondo

# Create app user
RUN useradd -m -s /bin/bash app

WORKDIR /app

# Copy dependency files first for better caching
COPY --chown=app:app deps.edn package.json package-lock.json* ./

# Create node_modules directory with correct permissions
RUN chown -R app:app /app

# Switch to app user
USER app

# Download dependencies
RUN clojure -P && \
    npm ci || npm install

# Copy source code
COPY --chown=app:app . .

# Note: Browser tests may fail in Docker due to Chrome sandbox restrictions.
# Run with: docker run --cap-add=SYS_ADMIN to enable Chrome sandbox
# Or run only non-browser tests: make cljtest nodetest
CMD ["make", "test"]