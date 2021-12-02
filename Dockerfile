FROM mcr.microsoft.com/dotnet/sdk:6.0

WORKDIR /code

RUN apt-get update && apt-get install -y make && \
    dotnet tool install --tool-path /usr/local/bin fantomas-tool && \
    dotnet tool install --tool-path /usr/local/bin dotnet-fsharplint

ENTRYPOINT ["make", "run"]