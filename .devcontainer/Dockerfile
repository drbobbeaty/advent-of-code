ARG VARIANT=focal
FROM mcr.microsoft.com/vscode/devcontainers/base:${VARIANT}

# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies.
#COPY library-scripts/*.sh /tmp/library-scripts/
#RUN bash chown -R vscode:vscode /home/vscode 

# [Optional] Uncomment this section to install additional OS packages.
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends openjdk-11-jdk leiningen \
	postgresql-client

# [Optional] Uncomment this line to install global node packages.
# RUN su vscode -c "source /usr/local/share/nvm/nvm.sh && npm install -g <your-package-here>" 2>&1

# ...and start off as the vscode user
USER vscode
