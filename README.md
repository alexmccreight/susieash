# susieash

## Description

{enter text later}

## Installation Instructions

To run susieash you must install the most recent version of susieR and mr.ash.alpha directly from their respective github repositories. First, you must install devtools by running the following command in your R console.

```R
install.packages("devtools")
```

### Installing susieR

Next, you must install [susieR](https://github.com/gaow/susieR).

```R
devtools::install_github("gaow/susieR")
```

### Installing mr.ash.alpha

Finally, you will need to install [mr.ash.alpha](https://github.com/stephenslab/mr.ash.alpha). Note that this package requires you to install gcc through [homebrew](https://brew.sh/).

First, you will install gcc.

```bash
brew install gcc
```

Second, you will create directory and add the following file.

```bash
mkdir -p ~/.R
touch ~/.R/Makevars
open -a TextEdit ~/.R/Makevars
```

Now, you will copy and paste the pathway to your gfortran. Note that the pathway may vary depending on your gcc installation

```bash
FLIBS=-L/opt/homebrew/lib/gcc/13
```

Finally, you will install the mr.ash.alpha package as usual.

```R
devtools::install_github("stephenslab/mr.ash.alpha")
```

### Installing susieash

After installing the dependencies, you can install susieash:

```R
devtools::install_github("alexmccreight/susieash")
```








