# pubiber

basic tool to extract reference from PubMed extract format and create a BibTeX file.

This is not a bibliography management tool per se, it's more of a quick tool to hack a bibtex file fro, a set of references and custom comments,
and more than that, it has been an interesting Haskell coding exercise =)


## exemple

given the file under `bibliography/references.txt`:

```
PMID - 7680325
NICKNAME - regression_asd_1993_epilepsy
COMMENT - propose direct role of epilepsy in asd regression

PMID - 28862395
NICKNAME - pms_dn_kothari
COMMENT - Initial PMS_DN article about integration of the different data sources

PMID - 11391650
NICKNAME - pms_phelan_2001
COMMENT - Identifying the PMS Syndrome

PMID - 7814313
NICKNAME - adi_r_description_lord_1994
COMMENT - describes the ADI-R
```

Running the command:

```
pubiber bibliography -i bibliography/references.txt -o bibliography/references.bib
```

Will query pubmed Entrez API and generate the following file, integrating the desired nickname and adding the comments:

```
@article{regression_asd_1993_epilepsy,
    author   ={Deonna, T and Ziegler, A L and Moura-Serra, J and Innocenti, G},
    title    ={Autistic regression in relation to limbic pathology and epilepsy: report of two cases.},
    publisher={Developmental medicine and child neurology},
    journal  ={Dev Med Child Neurol},
    date     ={1993},
    month    ={Feb},
    year     ={1993},
    comment  ={propose direct role of epilepsy in asd regression},
    keywords ={trusted},
}


@article{pms_dn_kothari,
    author   ={Kothari, Cartik and Wack, Maxime and Hassen-Khodja, Claire and Finan, Sean and Savova, Guergana and O'Boyle, Megan and Bliss, Geraldine and Cornell, Andria and Horn, Elizabeth J and Davis, Rebecca and Jacobs, Jacquelyn and Kohane, Isaac and Avillach, Paul},
    title    ={Phelan-McDermid syndrome data network: Integrating patient reported outcomes with clinical notes and curated genetic reports.},
    publisher={American journal of medical genetics. Part B, Neuropsychiatric genetics : the official publication of the International Society of Psychiatric Genetics},
    journal  ={Am J Med Genet B Neuropsychiatr Genet},
    date     ={2018},
    month    ={Oct},
    year     ={2018},
    comment  ={Initial PMS_DN article about integration of the different data sources},
    keywords ={trusted},
}


@article{pms_phelan_2001,
    author   ={Phelan, M C and Rogers, R C and Saul, R A and Stapleton, G A and Sweet, K and McDermid, H and Shaw, S R and Claytor, J and Willis, J and Kelly, D P},
    title    ={22q13 deletion syndrome.},
    publisher={American journal of medical genetics},
    journal  ={Am J Med Genet},
    date     ={2001},
    month    ={Jun},
    year     ={2001},
    comment  ={Identifying the PMS Syndrome},
    keywords ={trusted},
}


@article{adi_r_description_lord_1994,
    author   ={Lord, C and Rutter, M and Le Couteur, A},
    title    ={Autism Diagnostic Interview-Revised: a revised version of a diagnostic interview  for caregivers of individuals with possible pervasive developmental disorders.},
    publisher={Journal of autism and developmental disorders},
    journal  ={J Autism Dev Disord},
    date     ={1994},
    month    ={Oct},
    year     ={1994},
    comment  ={describes the ADI-R},
    keywords ={trusted},
}
```


