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

#### Running Baseprinter locally

Make sure Baseprinter and its dependencies are installed: [How to Install Baseprinter](https://try.perm.pub/baseprinter/howto/install/).

To generate only a Baseprint document snapshot, run:

```bash
$ make baseprint
```

To also generate an HTML/PDF preview, run:

```bash
$ make baseprint-preview
```

#### Running Baseprinter via OCI (Docker) container

To generated a Baseprint snapshot and HTML preview into `_output`,

using Pandoc:

```
podman/pandoc-baseprint.sh
```

#### Live preview

Optionally, you can run the following to live reload the HTML
preview whenever it changes.

```
podman/live-server.sh _output/pandoc/preview/
```
