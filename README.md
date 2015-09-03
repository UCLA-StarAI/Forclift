Summary
=======

Perform weighted model counting in a lifted manner.


Usage
=====

Information about the options and usage:

    $ java -jar ./wfomc.jar -h

For example, to perform inference on the example model `sickdeath.fg` for the
query `death`, you use the following command:

    $ java -jar ./wfomc.jar -q "death" models/sickdeath.fg

MLNs can be queried as follows:

    $ java -jar ./wfomc.jar -q "smokes(Guy)" models/friendsmoker.mln


Learning
========

To learn the weights of a theory, use the `--wl` flag and indicate the
training database(s) with the `-t` flag.

    $ java -jar ./wfomc.jar --mln-dist --wl 
        --train models/learning/smoking/smoking-train.db 
        models/learning/smoking/smoking.mln


External Dependencies
=====================

- For verifying the correctness of the results:
    * [c2d compiler](http://reasoning.cs.ucla.edu/c2d/): Adnan Darwiche's c2d 
      compiler.  
      Used for propositional inference and verification. The binary is assumed
      to be installed as `./c2d_linux`. This can be overridden with the
      environment variable `C2DCMD`.
- For visualizing the d-DNNFs:
    * [pdflatex](http://www.latex-project.org/)  
      `pdflatex` is assumed to be in your path.
    * [dot2tex](http://www.fauskes.net/code/dot2tex/)  
      `dot2tex` is assumed to be in your path.
    * [dot2texi](http://ftp.snt.utwente.nl/pub/software/tex/help/Catalogue/entries/dot2texi.html)  
      LaTeX package assumed to be installed.
    * [Graphviz](http://www.graphviz.org/)  
      The dot2tex tool expects  Graphviz `dot` to be available on your path.


Documentation
=============

See `doc/wfomc_manual.html` for the documentation.


Publications
============

The algorithms implemented are explained in the following publications:

- W. Meert, G. Van den Broeck, and A. Darwiche.
  Lifted inference for probabilistic logic programs.
  In Proceedings of the 1st Workshop on Probabilsitic Logic Programming (PLP), 2014.
  ([pdf](https://lirias.kuleuven.be/handle/123456789/460212))
- G. Van den Broeck, W. Meert, and A. Darwiche.
  Skolemization for Weighted First-Order Model Counting.
  In Proceedings of the 14th International Conference on Principles of Knowledge Representation and Reasoning (KR), 2014.
  ([pdf](https://lirias.kuleuven.be/handle/123456789/444264))
- G. Van den Broeck, W. Meert and J. Davis.
  Lifted Generative Parameter Learning.
  In Proceedings of the 3rd International workshop on Statistical Relational AI (StarAI), held at the 27th AAAI Conference, 2013.
- G. Van den Broeck.
  Lifted Inference and Learning in Statistical Relational Models.
  PhD dissertation KU Leuven, 2013.
  ([pdf](https://lirias.kuleuven.be/handle/123456789/373041))
- G. Van den Broeck and J. Davis.
  Conditioning in First-Order Knowledge Compilation and Lifted Probabilistic Inference.
  In Proceedings of the 26th AAAI Conference on Artificial Intelligence (AAAI), 2012.
  ([pdf](https://lirias.kuleuven.be/handle/123456789/345667))
- G. Van den Broeck, A. Choi, A. Darwiche. 
  Lifted relax, compensate and then recover: From approximate to exact lifted probabilistic inference.
  In Proceedings of the conference on Uncertainty in Artificial Intelligence (UAI), 2012
  ([pdf](https://lirias.kuleuven.be/handle/123456789/351575))
- M. Jaeger, G. Van den Broeck. 
  Liftability of probabilistic inference: Upper and lower bounds.
  In Proceedings of the 2nd International Workshop on Statistical Relational AI (StarAI), 2012.
  ([pdf](https://lirias.kuleuven.be/handle/123456789/352388))
- W. Meert, G. Van den Broeck, N. Taghipour, D. Fierens, H. Blockeel, J. Davis, L. De Raedt. 
  Lifted inference for probabilistic programming.
  In Proceedings of the NIPS Probabilistic Programming Workshop, 2012.
  ([pdf](https://lirias.kuleuven.be/handle/123456789/369419))
- G. Van den Broeck.
  On the completeness of first-order knowledge compilation for lifted probabilistic inference.
  In Proceedings of the Annual Conference on Neural Information Processing Systems (NIPS), 2011
  ([pdf](https://lirias.kuleuven.be/handle/123456789/316338))
- G. Van den Broeck, N. Taghipour, W. Meert, J. Davis, and L. De Raedt.
  Tutorial on Lifted Inference in Probabilistic Logical Models.
  On the 22th International Joint Conference on Artificial Intelligence (IJCAI), 2011. 
  ([pdf](https://lirias.kuleuven.be/handle/123456789/317055))
- G. Van den Broeck, N. Taghipour, W. Meert, J. Davis, and L. De Raedt.
  Lifted probabilistic inference by first-order knowledge compilation.
  In Proceedings of the 22th International Joint Conference on Artificial Intelligence (IJCAI), 2011.
  ([pdf](https://lirias.kuleuven.be/handle/123456789/308265))


Contact
=======

<http://dtai.cs.kuleuven.be/wfomc/>

**Main contact:**  
Guy Van den Broek  
Department of Computer Science  
KU Leuven  
Celestijnenlaan 200A  
3001 Leuven  
<http://www.guyvdb.eu>  
<guy.vandenbroeck@cs.kuleuven.be>


Contributors
============

- [Guy Van den Broeck](http://www.guyvdb.eu)
- [Wannes Meert](http://people.cs.kuleuven.be/wannes.meert)
- [Jesse Davis](http://people.cs.kuleuven.be/jesse.davis)
- [Jan Van Haaren](http://people.cs.kuleuven.be/jan.vanhaaren)


Credits
=======

- [Argot](http://software.clapper.org/argot/):  
  Used for command line parsing  
  BSD License
- [Breeze](https://github.com/scalanlp/breeze):  
  Used for optimization and complex numbers  
  Apache 2.0 License


License
=======

Licensed under the Apache License, Version 2.0: 
http://www.apache.org/licenses/LICENSE-2.0

Copyright (c) 2011-2015, KU Leuven. All rights reserved.