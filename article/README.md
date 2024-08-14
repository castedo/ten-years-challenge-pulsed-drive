<!-- copybreak off -->
### [ReScience C](https://rescience.github.io/) article template

This repository contains the Latex (optional) template for writing a ReScience
C article and the (mandatory) YAML metadata file. For the actual article,
you're free to use any software you like as long as you enforce the proposed
PDF style. A tool is available for the latex template that produces latex
definitions from the metadata file. If you use another software, make sure that
metadata and PDF are always synced.

You can also use overleaf with the [provided template](https://www.overleaf.com/read/kfrwdmygjyqw) but in this case, you'll have to enter `metadata.tex` manually.

#### Usage

For a submission, fill in information in
[metadata.yaml](./metadata.yaml), modify [content.tex](content.tex)
and type:

```bash
$ make 
```

This will produce an `article.pdf` using xelatex and provided font. Note that you must have Python 3 and [PyYAML](https://pyyaml.org/) installed on your computer, in addition to `make`.


After acceptance, you'll need to complete [metadata.yaml](./metadata.yaml) with information provided by the editor and type again:

```bash
$ make
```

### Generate a Baseprint snapshot and HTML preview

[Baseprinter](https://try.perm.pub/baseprinter/) can be used to generate a Baseprint document snapshot and HTML preview.
It can be run locally or via an OCI (Docker) container.


<!-- copybreak on -->
#### Running Baseprinter via OCI (Docker) container

Until a Debian package is available, running via a container will likely be easier.
You can do this with the `with-podman.sh` script located in this directory, followed by one of
the commands you would run locally, as described in the next section. For example:

```
./with-podman.sh make baseprint-preview
```
or

```
./with-podman.sh make baseprint-live
```

#### Running Baseprinter locally

Ensure that Baseprinter and its dependencies are installed: [How to Install Baseprinter](https://try.perm.pub/baseprinter/howto/install/).

To generate only a Baseprint document snapshot, run:

```bash
$ make baseprint
```

To also generate an HTML/PDF preview, run:

```bash
$ make baseprint-preview
```

To generate and reload an HTML preview each time the source changes:

```bash
$ make baseprint-live
```
