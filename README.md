# Ergo

This repository contains the reference implementation of the
Ergo Platform protocol, which is an alternative to
the [Bitcoin protocol](https://bitcoin.org/bitcoin.pdf).

Ergo Platform website: [https://ergoplatform.org/](https://ergoplatform.org/)

## Differences from Bitcoin

* Powerful contracts in the multi-stage extended UTXO model (see [ErgoScript whitepaper](https://ergoplatform.org/docs/ErgoScript.pdf)) 
* Memory-hard non-outsourceable Proof-of-Work function [Autolykos](https://github.com/ergoplatform/autoleakus)
* New modes of operation: [light-fullnode](https://eprint.iacr.org/2016/994),
[light-SPV](http://fc16.ifca.ai/bitcoin/papers/KLS16.pdf), hybrid modes
* [Alternative transactional language](https://github.com/ScorexFoundation/sigmastate-interpreter), which is more powerful that Bitcoin Script but also safe against
heavy validation attacks
* Alternative fee model with [mandatory storage-rent component](https://fc18.ifca.ai/bitcoin/papers/bitcoin18-final18.pdf )

## Specifications

A [White Paper](https://ergoplatform.org/docs/whitepaper.pdf) with a brief description is available. A Yellow Paper with detailed specification is underway and will be available shortly. At the moment, there are [drafts of the Yellow Paper](https://github.com/ergoplatform/ergo/tree/master/papers/yellow) available,
and currently the reference implementation code should be considered as the specification.

## Installation

You can check our [Setup A Full Node](https://github.com/ergoplatform/ergo/wiki/Set-up-a-full-node) wiki page to learn how to manually setup and configure a node.

Alternatively you can run the prepared [ergo-installer.sh](ergo-installer.sh) script. With this script you'll have the latest Ergo node installed without any hassle (only availalbe for Linux distributions):

    curl -s https://raw.githubusercontent.com/ergoplatform/ergo/master/ergo-installer.sh | sh -s -- --api-key=<YOUR_API_KEY>

## Build from source

In order to build the Ergo node from sources you need JDK (>= 1.8) and SBT to be 
[installed](https://docs.scala-lang.org/getting-started/sbt-track/getting-started-with-scala-and-sbt-on-the-command-line.html) on your machine.

In order to simply get a single jar run: `sbt assembly` - assembly would appear in `target/scala-2.12/` directory.
 
If you want to create a package for a specific platform with launching scripts the one of the following 
packager commands could be chosen (depending on desired system type you want to build for):
 - `universal:packageBin` - Generates a universal zip file
 - `universal:packageZipTarball` - Generates a universal tgz file
 - `debian:packageBin` - Generates a deb
 - `docker:publishLocal` - Builds a Docker image using the local Docker server
 - `rpm:packageBin` - Generates an rpm
 - `universal:packageOsxDmg` - Generates a DMG file with the same contents as the universal zip/tgz.
 - `windows:packageBin` - Generates an MSI
 
 The final build command should look like: `sbt <packager_command>`, example: `sbt universal:packageBin`.
 A resulted package could be found in the `target/scala-2.12/<platform_type>` directory.

## Running the node

The node can be started in a couple different ways:
 
 - In case you only have a jar: `java -jar /path/to/ergo-<version>.jar --<networkId> -c /path/to/local.conf`
 - Using start script from sbt-native-packager: `sh /path/to/bin/ergo  --<networkId> -c /path/to/local.conf`
 
Available `networkId` options: `mainnet`, `testnet`, `devnet`.

## UI

Node UI (graphical interface) could be accessed at `<node_ip>:<api_port>/panel` in your browser.

<img src="https://github.com/ergoplatform/static-data/raw/master/img/node_ui.png" align="right" />

## Docker Quick Start

Ergo has officially supported Docker package. To run last Ergo version in mainnet as a console application with logs printed to console:

    sudo docker run --rm -p 9030:9030 -p 127.0.0.1:9053:9053 -v /path/on/host/to/ergo/data:/home/ergo/.ergo ergoplatform/ergo --mainnet

This will connect to Ergo mainnet with default config and open port `9030` globally and `9053` locally on the host system. All data will be stored in your host directory `/path/on/host/to/ergo/data`.

To run specific Ergo version `<VERSION>` as a service with custom config `/path/on/host/system/to/myergo.conf`:

    sudo docker run -d \
        -p 9030:9030 \
        -p 127.0.0.1:9053:9053 \
        -v /path/on/host/to/ergo/data:/home/ergo/.ergo \
        -v /path/on/host/system/to/myergo.conf:/etc/myergo.conf \
        -e MAX_HEAP=3G \
        ergoplatform/ergo:<VERSION> --<networkId> -c /etc/myergo.conf

Available versions can be found on [Ergo Docker image page](https://hub.docker.com/r/ergoplatform/ergo/tags), for example, `v4.0.12`.

This will connect to the Ergo mainnet or testnet following your configuration passed in `myergo.conf` and network flag `--<networkId>`. Every default config value would be overwritten with corresponding value in `myergo.conf`. `MAX_HEAP` variable can be used to control how much memory can the node consume.

This command also would store your data in `/path/on/host/to/ergo/data` on host system, and open ports `9030` (node communication) globally and `9053` (REST API) locally on host system. The `/path/on/host/to/ergo/data` directory must has `777` permissions or has owner/group numeric id equal to `9052` to be writable by container, as `ergo` user inside Docker image (please refer to [Dockerfile](Dockerfile)).

Ergo node works normally behind NAT, so you can keep closed your `9030` port, hence other nodes could not discover and connect to yours one, only your node could initiate connections.

It is also a good practice to keep closed REST API port `9053`, and connect to your node from inside another container in the same Docker network (this case not covered by this short quick start manual).

## Testing

There are three kinds of tests: 

1) Unit and property tests, run them with `sbt test` command.
2) Integration tests, they require for Docker to be installed, then run `sudo sbt it:test`.
3) Bootstrapping tests, very slow as they are checking that the node is indeed catching up with the main network in 
different regimes, they require for Docker too, run as `sudo sbt it2:test`.

## Open project in IDE

Your can use [IntelliJ IDEA](https://www.jetbrains.com/idea/) (Community or Ultimate edition) or 
[VSCode](https://code.visualstudio.com/) + [Metals](https://scalameta.org/metals/).
Before opening the project in IDE make sure it can be built with sbt. 
You may need to fix dependency resolution errors first.

After that you can open the project folder in Idea (File / Open)
which will run Project Import Wizard. The wizard will use SBT configuration
(build.sbt file) to generate Idea's project configuration files.
You can open `File / Project Structure...` dialog to see project configuration.
If everything is successful you can compile the project in IDE. 

## FAQ
[Frequently Asked Questions](FAQ.md)

## Acknowledgements

<img src="https://www.yourkit.com/images/yklogo.png" align="right" />

YourKit supports open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of <a href="https://www.yourkit.com/java/profiler/">YourKit Java Profiler</a>
and <a href="https://www.yourkit.com/.net/profiler/">YourKit .NET Profiler</a>,
innovative and intelligent tools for profiling Java and .NET applications.
