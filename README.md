# Les-Herbiers

## Informations de connexion
Si vous n'avez pas apporté de modifications à ces informations, l'application dispose d'un
utilisateur par défaut dont le rôle est Administrateur. Ses informations de connexion sont les
suivantes :

| login   | mot de passe |
|---------|--------------|
| `admin` | `admin`      |

Si vous avez modifié ces informations et ne vous en souvenez plus, vous pouvez éventuellement
tenter de les modifier ou de les retrouver en allant lire le contenu du fichier utilisateur.dat,
mais il n'est pas prévu dans l'application d'autre moyen de les retrouver.

## Informations sur la compilation et l'exécution
Pour compiler le programme, déplacez votre terminal à la racine du projet (là où se trouve le
fichier `herbier.cob`) et exécutez la commande suivante :
```bash
cobc -free -x herbier.cob
```

Enfin, pour exécuter le logiciel, il suffit d'exécuter le fichier `herbier` généré à l'issue de
la compilation :
```bash
./herbier
```
