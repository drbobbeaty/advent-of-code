# Update the VARIANT arg in docker-compose.yml to pick Ubuntu version
ARG VARIANT=focal
FROM mcr.microsoft.com/vscode/devcontainers/base:${VARIANT}

# [Optional] Uncomment this section to install additional OS packages.
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
     && apt-get -y install --no-install-recommends openjdk-11-jdk leiningen \
	 acl postgresql-client

# Make my user for when we really get going, and pull in my env from GitHub
ARG USERNAME=drbob
ARG USER_UID=1001
ARG USER_GID=$USER_UID

# Create the user and add him to the sudoers so that he can install more stuff
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    && apt-get update \
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

# ********************************************************
# * Anything else you want to do like clean up goes here *
# ********************************************************

# [Optional] Set the default user. Omit if you want to keep the default as root.
USER $USERNAME
# USER vscode

