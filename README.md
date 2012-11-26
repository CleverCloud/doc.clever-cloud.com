# documentation clever cloud

## écrire un post

créer un fichier `.md` dans le dossier `/_posts`. vous pouvez le mettre dans
un des sous-dossiers présents, ça évitera que ce soit trop le bordel, mais ça
n'a aucune conséquence pour la suite.

le fichier doit être nommé en `YYYY-MM-DD-nom-du-fichier.md`, c'est moche mais
on a pas le choix, ce qui signifie que les posts avec la date la plus récente
seront affichés au début dans les listes.

dans les méta-données le `title` est facultatif mais souhaitable (sinon ce
sera nom-du-fichier qui sera changé magiquement en Nom Du Fichier et utilisé)

le tag en revanche détermine ou ce sera rangé, collez celui qui correspond et
hop, normalement c'est bon.

pour l'essentiel et histoire de pas se casser la nénète, un copier-coller
d'un autre post est de bon goût.

attention quand même :

* les tags sont case-sensitive, pas de boulettes svp.

* évitez au maximum les posts qui portent le même nom, j'ai aucune idée de
  ce que ça ferait mais ça sent mauvais.

* j'ai rangé les posts un peu what the fuck ou sur des technos pas encore
  sorties dans `/_posts/waiting`. de manière générale, tout post qui n'a pas
  le format indiqué plus haut ne sera pas généré.

* enfin, l'url de la page sera `http://doc.clever-cloud.com/nom-du-fichier/`
  donc on fait pas trop les cons avec les noms :D

## nouvelle catégorie ou update

le fichier `_config.yml` donne l'accès à une liste de tags triés, ça évite que
l'ordre dans la sidebar soit aléatoire à la génération (héé oui). vous êtes
libres de changer l'ordre, d'en rajouter, etc.

il y a en prime un booleen pour décider si on veut afficher les autres tags à
la suite dans la sidebar ou pas.
