
<!-- README.md is generated from README.Rmd. Please edit that file -->

## procesbeheer-begrazing

Langetermijn-monitoring van de processen in kerngebieden die
representatief zijn voor onbeheerde climaxvegetaties of door begrazing
gestuurde mozaïeklandschappen

Project dat test welke fouten er zijn tussen vegetatiehoogtekaarten van herhaalde vluchten en tussen vegetatiehoogtekaarten en manuele vegetatiehoogtemetingen Het doel van deze proef is om na te gaan of fotografische beelden, ingezameld via drones, kunnen gebruikt worden om de vegetatiestructuur van open biotopen in onze biogeografische regio te onderzoeken. We gaan na of de beeldverwerkingstechniek: fotogrammetrie, en meer specifiek SfM modellering, kan gebruikt worden om complexe vegetatiestructuren die ontstaan onder extensieve begrazing in kaart te brengen. Het potentieel van deze techniek voor lage vegetatie, met een hoge ruimtelijke heterogeniteit en structurele complexiteit werd o.a. al aangetoond door (Fraser et al. 2016) voor arctische toendra vegetaties. Uit deze proef willen we leren om zo een goede staalnamestrategie uit te werken voor de onderzoeksgebieden. Specifiek heeft deze proef vijf doelstelingen: Doelstelling 1) Bepalen wat de optimale vlieghoogte is in functie van de gewenste nauwkeurigheid van het CHM (canopy height model) en de te bemonsteren oppervlakte; Doelstelling 2) Een geschikte methodiek ontwikkelen voor het meten van de vegetatiehoogte op het terrein zodat deze terreinmetingen kunnen gebruikt worden om de drone-data te valideren; Doelstelling 3) Nagaan wat de verschillen in x-, y- en z-richting zijn tussen twee vluchten met dezelfde vluchtparameters als gevolg van omgevingsfactoren zoals wind en de herhaling op zich; Doelstelling 4) Verklaren van de overeenstemming/verschillen tussen de CHM’s (Conopy Height Modellen), gemaakt via fotogrammetrische verwerking en de veldinventarisatie van de vegetatiehoogte; Doelstelling 5) Nagaan welke grootte van vegetatiehoogteverschillen kunnen gedetecteerd worden in opeenvolgende vluchten met dezelfde vluchtparameters en onder vergelijkbare omgevingscondities.

## Structuur van de repository

    #> .
    #> +-- data
    #> +-- dronebeelden
    #> \-- src

## De repository lokaal installeren

  - ga naar [de
    repository](https://github.com/inbo/procesbeheer-begrazing/) en klik
    op Code en kopieer vervolgens de link naar het klembord
  - start RStudio en selecteer `File -> New project -> Version Control
    -> Git` -\> paste the URL
  - `procesbeheer-begrazing` zal automatisch gesuggereerd worden als
    project foldernaam
  - In het veld `Create project as subdirectory of` selecteer een map op
    je lokale harde schijf. Bijvoorbeeld `C:/R/GitRepositories`.
  - klik OK

Je hebt nu een lokale kloon van de remote repository beschikbaar als een
RStudio project. Dezelfde mappen en bestanden die je kan zien op de
[remote](https://github.com/inbo/procesbeheer-begrazing) worden
gekopieerd naar je lokale harde schijf. Een `.git` map wordt eveneens
aangemaakt (hier mag niets in gewijzigd worden).

Wanneer je wil werken aan het project, open je het bestand
`procesbeheer-begrazing.Rproj` met RStudio.

## Versiebeheer

Een goede manier om samen te werken in git is [volgens deze
workflow](https://inbo.github.io/git-course/workflow_rstudio.html).

## Opslaan en delen van grote bestanden

Git is niet geschikt voor het opslaan en delen van grote (binaire)
bestanden, of voor het versiebeheer ervan.

Omdat binnen dit project, gebruik wordt gemaakt van dronebeelden (tif
bestanden van enkele gigabyte), zullen we gebruik maken van een gedeelde
google drive folder.

Met behulp van het `googledrive` package, kunnen deze bestanden
gedownload worden naar de map `dronebeelden/`. Alle bestanden in deze
map worden genegeerd door het versiebeheer.

## Welke mappen en bestanden staan niet onder versiebeheer?

Sommige bestanden, bestandstypes en mappen worden niet getraceerd door
het git controlesysteem. Welke dit zijn staat in [het .gitignore
bestand](.gitignore).
