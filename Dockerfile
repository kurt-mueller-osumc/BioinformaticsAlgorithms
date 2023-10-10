FROM mcr.microsoft.com/vscode/devcontainers/dotnet:7.0

# [Option] Install Node.js
ARG INSTALL_NODE="true"
ARG NODE_VERSION="lts/*"

ENV WORKDIR="/app" \
    USER="vscode"
# RUN if [ "${INSTALL_NODE}" = "true" ]; then su vscode -c "umask 0002 && . /usr/local/share/nvm/nvm.sh && nvm install ${NODE_VERSION} 2>&1"; fi

COPY --chown=${USER}:${USER} . ${WORKDIR}

WORKDIR ${WORKDIR}

RUN dotnet tool restore && \
    dotnet paket install && \
    dotnet restore

# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>

# [Optional] Uncomment this line to install global node packages.
# RUN su vscode -c "source /usr/local/share/nvm/nvm.sh && npm install -g <your-package-here>" 2>&1