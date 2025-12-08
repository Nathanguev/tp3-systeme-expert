// =============================================================================
// SYSTÈME EXPERT CULINAIRE - JAVASCRIPT
// =============================================================================

// -----------------------------------------------------------------------------
// CONFIGURATION
// -----------------------------------------------------------------------------
const API_BASE_URL = 'http://localhost:8080/api';

// État global de l'application
const state = {
    ingredients: [],
    materiel: [],
    ingredientsSelectionnes: new Map(), // Map<nom, quantite>
    materielSelectionne: new Set(),
    recettes: [],
    trace: []
};

// -----------------------------------------------------------------------------
// INITIALISATION
// -----------------------------------------------------------------------------
document.addEventListener('DOMContentLoaded', () => {
    console.log('🚀 Initialisation de l\'application');
    chargerIngredients();
    chargerMateriel();
});

// -----------------------------------------------------------------------------
// GESTION DES ONGLETS
// -----------------------------------------------------------------------------
function afficherOnglet(nomOnglet) {
    // Retirer la classe 'active' de tous les onglets et boutons
    document.querySelectorAll('.tab-content').forEach(tab => {
        tab.classList.remove('active');
    });
    document.querySelectorAll('.tab-btn').forEach(btn => {
        btn.classList.remove('active');
    });
    
    // Ajouter la classe 'active' à l'onglet et bouton correspondants
    const tabElement = document.getElementById(`tab-${nomOnglet}`);
    if (tabElement) {
        tabElement.classList.add('active');
    }
    
    // Activer le bouton correspondant
    const buttons = document.querySelectorAll('.tab-btn');
    buttons.forEach((btn, index) => {
        const tabs = ['ingredients', 'materiel', 'filtres', 'recettes', 'trace'];
        if (tabs[index] === nomOnglet) {
            btn.classList.add('active');
        }
    });
    
    // Si on affiche l'onglet trace, mettre à jour l'affichage
    if (nomOnglet === 'trace') {
        afficherTrace();
    }
}

// -----------------------------------------------------------------------------
// API - APPELS HTTP
// -----------------------------------------------------------------------------

/**
 * Effectue un appel GET à l'API
 * @param {string} endpoint - Point de terminaison de l'API
 * @returns {Promise<Object>} Réponse JSON
 */
async function apiGet(endpoint) {
    try {
        const response = await fetch(`${API_BASE_URL}${endpoint}`);
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        return await response.json();
    } catch (error) {
        console.error('Erreur API GET:', error);
        throw error;
    }
}

/**
 * Effectue un appel POST à l'API
 * @param {string} endpoint - Point de terminaison de l'API
 * @param {Object} data - Données à envoyer
 * @returns {Promise<Object>} Réponse JSON
 */
async function apiPost(endpoint, data = {}) {
    try {
        const url = `${API_BASE_URL}${endpoint}`;
        const params = new URLSearchParams(data).toString();
        const fullUrl = params ? `${url}?${params}` : url;
        
        const response = await fetch(fullUrl, {
            method: 'POST'
        });
        
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        return await response.json();
    } catch (error) {
        console.error('Erreur API POST:', error);
        throw error;
    }
}

// -----------------------------------------------------------------------------
// INGRÉDIENTS
// -----------------------------------------------------------------------------

/**
 * Charge la liste des ingrédients depuis l'API
 */
async function chargerIngredients() {
    try {
        const response = await apiGet('/ingredients');
        if (response.success && response.data) {
            state.ingredients = response.data;
            afficherIngredients();
        } else {
            throw new Error('Format de réponse invalide');
        }
    } catch (error) {
        console.error('Erreur lors du chargement des ingrédients:', error);
        document.getElementById('liste-ingredients').innerHTML = 
            '<p class="error">Erreur: Impossible de charger les ingrédients. Le serveur est-il démarré?</p>';
        gererErreur(error);
    }
}

/**
 * Affiche la liste des ingrédients dans le DOM
 */
function afficherIngredients() {
    const conteneur = document.getElementById('liste-ingredients');
    conteneur.innerHTML = '';
    
    if (state.ingredients.length === 0) {
        conteneur.innerHTML = '<p class="info">Aucun ingrédient disponible.</p>';
        return;
    }
    
    state.ingredients.forEach(ingredient => {
        const card = document.createElement('div');
        card.className = 'item-card';
        
        // Header avec checkbox et label
        const header = document.createElement('div');
        header.className = 'item-card-header';
        
        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.id = `ing-${ingredient}`;
        checkbox.onchange = (e) => {
            const qteInput = document.getElementById(`qte-${ingredient}`);
            qteInput.disabled = !e.target.checked;
            
            if (e.target.checked) {
                card.classList.add('selected');
                // Mettre la quantité à 1 par défaut lors de la sélection
                qteInput.value = '1';
                state.ingredientsSelectionnes.set(ingredient, 1);
            } else {
                card.classList.remove('selected');
                // Remettre la quantité à 0 lors de la désélection
                qteInput.value = '0';
                state.ingredientsSelectionnes.delete(ingredient);
            }
        };
        
        const label = document.createElement('label');
        label.htmlFor = `ing-${ingredient}`;
        label.textContent = formatterNom(ingredient);
        
        header.appendChild(checkbox);
        header.appendChild(label);
        
        // Section quantité
        const quantitySection = document.createElement('div');
        quantitySection.className = 'item-card-quantity';
        
        const quantityLabel = document.createElement('label');
        quantityLabel.textContent = 'Quantité :';
        quantityLabel.htmlFor = `qte-${ingredient}`;
        
        const quantite = document.createElement('input');
        quantite.type = 'number';
        quantite.id = `qte-${ingredient}`;
        quantite.min = '0';
        quantite.value = '0';
        quantite.placeholder = '0';
        quantite.disabled = true;
        quantite.onchange = () => {
            if (checkbox.checked) {
                const qte = parseInt(quantite.value) || 0;
                if (qte > 0) {
                    state.ingredientsSelectionnes.set(ingredient, qte);
                } else {
                    state.ingredientsSelectionnes.delete(ingredient);
                    checkbox.checked = false;
                    card.classList.remove('selected');
                }
            }
        };
        
        quantitySection.appendChild(quantityLabel);
        quantitySection.appendChild(quantite);
        
        // Rendre toute la carte cliquable (sauf l'input de quantité)
        card.onclick = (e) => {
            // Ne pas déclencher si on clique sur l'input de quantité
            if (e.target === quantite) {
                return;
            }
            // Ne pas déclencher si on clique sur la checkbox (elle gère déjà son propre état)
            if (e.target === checkbox) {
                return;
            }
            // Inverser l'état de la checkbox
            checkbox.checked = !checkbox.checked;
            checkbox.dispatchEvent(new Event('change'));
        };
        
        card.appendChild(header);
        card.appendChild(quantitySection);
        conteneur.appendChild(card);
    });
}

/**
 * Filtre les ingrédients selon la recherche
 */
function filtrerIngredients() {
    const recherche = document.getElementById('search-ingredients').value.toLowerCase();
    const cards = document.querySelectorAll('#liste-ingredients .item-card');
    
    cards.forEach(card => {
        const label = card.querySelector('label');
        const texte = label.textContent.toLowerCase();
        
        if (texte.includes(recherche)) {
            card.style.display = 'flex';
        } else {
            card.style.display = 'none';
        }
    });
}

/**
 * Valide et envoie les ingrédients sélectionnés à l'API
 */
async function validerIngredients() {
    if (state.ingredientsSelectionnes.size === 0) {
        afficherNotification('Veuillez sélectionner au moins un ingrédient', 'warning');
        return;
    }
    
    try {
        for (const [ingredient, quantite] of state.ingredientsSelectionnes.entries()) {
            await apiPost('/ingredients/add', { ingredient, quantite: quantite.toString() });
        }
        
        afficherNotification(`${state.ingredientsSelectionnes.size} ingrédient(s) ajouté(s)`, 'success');
        afficherOnglet('materiel');
    } catch (error) {
        afficherNotification('Erreur lors de l\'ajout des ingrédients', 'error');
        gererErreur(error);
    }
}

/**
 * Réinitialise la sélection d'ingrédients
 */
function reinitialiserIngredients() {
    state.ingredientsSelectionnes.clear();
    
    document.querySelectorAll('#liste-ingredients .item-card').forEach(card => {
        card.classList.remove('selected');
    });
    
    document.querySelectorAll('#liste-ingredients input[type="checkbox"]').forEach(cb => {
        cb.checked = false;
    });
    
    document.querySelectorAll('#liste-ingredients input[type="number"]').forEach(input => {
        input.value = '0';
        input.disabled = true;
    });
    
    afficherNotification('Sélection réinitialisée', 'success');
}

// -----------------------------------------------------------------------------
// MATÉRIEL
// -----------------------------------------------------------------------------

/**
 * Charge la liste du matériel depuis l'API
 */
async function chargerMateriel() {
    try {
        const response = await apiGet('/materiel');
        if (response.success && response.data) {
            state.materiel = response.data;
            afficherMateriel();
        } else {
            throw new Error('Format de réponse invalide');
        }
    } catch (error) {
        console.error('Erreur lors du chargement du matériel:', error);
        document.getElementById('liste-materiel').innerHTML = 
            '<p class="error">Erreur: Impossible de charger le matériel.</p>';
        gererErreur(error);
    }
}

/**
 * Affiche la liste du matériel dans le DOM
 */
function afficherMateriel() {
    const conteneur = document.getElementById('liste-materiel');
    conteneur.innerHTML = '';
    
    if (state.materiel.length === 0) {
        conteneur.innerHTML = '<p class="info">Aucun matériel disponible.</p>';
        return;
    }
    
    state.materiel.forEach(materiel => {
        const card = document.createElement('div');
        card.className = 'item-card';
        
        // Header avec checkbox et label
        const header = document.createElement('div');
        header.className = 'item-card-header';
        
        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.id = `mat-${materiel}`;
        checkbox.onchange = (e) => {
            if (e.target.checked) {
                card.classList.add('selected');
                state.materielSelectionne.add(materiel);
            } else {
                card.classList.remove('selected');
                state.materielSelectionne.delete(materiel);
            }
        };
        
        const label = document.createElement('label');
        label.htmlFor = `mat-${materiel}`;
        label.textContent = formatterNom(materiel);
        
        header.appendChild(checkbox);
        header.appendChild(label);
        
        // Rendre toute la carte cliquable
        card.onclick = (e) => {
            // Ne pas déclencher si on clique sur la checkbox (elle gère déjà son propre état)
            if (e.target === checkbox) {
                return;
            }
            // Inverser l'état de la checkbox
            checkbox.checked = !checkbox.checked;
            checkbox.dispatchEvent(new Event('change'));
        };
        
        card.appendChild(header);
        conteneur.appendChild(card);
    });
}

/**
 * Filtre le matériel selon la recherche
 */
function filtrerMateriel() {
    const recherche = document.getElementById('search-materiel').value.toLowerCase();
    const cards = document.querySelectorAll('#liste-materiel .item-card');
    
    cards.forEach(card => {
        const label = card.querySelector('label');
        const texte = label.textContent.toLowerCase();
        
        if (texte.includes(recherche)) {
            card.style.display = 'flex';
        } else {
            card.style.display = 'none';
        }
    });
}

/**
 * Valide et envoie le matériel sélectionné à l'API
 */
async function validerMateriel() {
    if (state.materielSelectionne.size === 0) {
        afficherNotification('Veuillez sélectionner au moins un équipement', 'warning');
        return;
    }
    
    try {
        for (const materiel of state.materielSelectionne) {
            await apiPost('/materiel/add', { materiel });
        }
        
        afficherNotification(`${state.materielSelectionne.size} équipement(s) ajouté(s)`, 'success');
        afficherOnglet('filtres');
    } catch (error) {
        afficherNotification('Erreur lors de l\'ajout du matériel', 'error');
        gererErreur(error);
    }
}

/**
 * Réinitialise la sélection de matériel
 */
function reinitialiserMateriel() {
    state.materielSelectionne.clear();
    
    document.querySelectorAll('#liste-materiel .item-card').forEach(card => {
        card.classList.remove('selected');
    });
    
    document.querySelectorAll('#liste-materiel input[type="checkbox"]').forEach(cb => {
        cb.checked = false;
    });
    
    afficherNotification('Sélection réinitialisée', 'success');
}

// -----------------------------------------------------------------------------
// FILTRES
// -----------------------------------------------------------------------------

/**
 * Valide et envoie les filtres à l'API
 */
async function validerFiltres() {
    try {
        const vegetarien = document.getElementById('filtre-vegetarien')?.checked ? 'true' : 'false';
        
        const saisonsChecked = document.querySelectorAll('input[name="saison"]:checked');
        const saisons = Array.from(saisonsChecked).map(cb => cb.value).join(',');
        
        const typesChecked = document.querySelectorAll('input[name="type"]:checked');
        const types = Array.from(typesChecked).map(cb => cb.value).join(',');
        
        await apiPost('/filtres', { vegetarien, saisons, types });
        
        afficherNotification('Filtres configurés', 'success');
        afficherOnglet('recettes');
    } catch (error) {
        afficherNotification('Erreur lors de la configuration des filtres', 'error');
        gererErreur(error);
    }
}

/**
 * Réinitialise tous les filtres
 */
function reinitialiserFiltres() {
    const vegCheckbox = document.getElementById('filtre-vegetarien');
    if (vegCheckbox) vegCheckbox.checked = false;
    
    document.querySelectorAll('input[name="saison"]').forEach(cb => cb.checked = false);
    document.querySelectorAll('input[name="type"]').forEach(cb => cb.checked = false);
    
    afficherNotification('Filtres réinitialisés', 'success');
}

// -----------------------------------------------------------------------------
// RECHERCHE DE RECETTES
// -----------------------------------------------------------------------------

/**
 * Lance la recherche de recettes (chaînage avant)
 */
async function lancerRecherche() {
    const conteneur = document.getElementById('resultats-recettes');
    conteneur.innerHTML = '<p class="loading">Recherche en cours...</p>';
    
    try {
        const response = await apiPost('/recherche');
        
        if (response.success && response.data) {
            state.recettes = response.data.recettes || [];
            state.trace = response.data.trace || [];
            
            afficherRecettes();
            afficherNotification(`${state.recettes.length} recette(s) trouvée(s)`, 'success');
        } else {
            throw new Error('Format de réponse invalide');
        }
    } catch (error) {
        conteneur.innerHTML = '<p class="error">Erreur lors de la recherche de recettes.</p>';
        afficherNotification('Erreur lors de la recherche', 'error');
        gererErreur(error);
    }
}

/**
 * Affiche les recettes trouvées
 */
function afficherRecettes() {
    const conteneur = document.getElementById('resultats-recettes');
    conteneur.innerHTML = '';
    
    if (state.recettes.length === 0) {
        conteneur.innerHTML = '<p class="info">Aucune recette trouvée avec les ingrédients et filtres sélectionnés.</p>';
        return;
    }
    
    // Grouper par type
    const entrees = state.recettes.filter(r => r.type === 'entree');
    const plats = state.recettes.filter(r => r.type === 'plat');
    const desserts = state.recettes.filter(r => r.type === 'dessert');
    
    const afficherGroupe = (titre, recettes, emoji) => {
        if (recettes.length === 0) return;
        
        const section = document.createElement('div');
        section.className = 'recettes-groupe';
        
        const h3 = document.createElement('h3');
        h3.textContent = `${emoji} ${titre}`;
        section.appendChild(h3);
        
        recettes.forEach(recette => {
            const card = document.createElement('div');
            card.className = 'recette-card';
            
            const titre = document.createElement('h4');
            titre.textContent = formatterNom(recette.nom);
            card.appendChild(titre);
            
            if (recette.description) {
                const desc = document.createElement('p');
                desc.textContent = recette.description;
                card.appendChild(desc);
            }
            
            const badges = document.createElement('div');
            badges.className = 'badges';
            
            if (recette.vegetarien) {
                const badge = document.createElement('span');
                badge.className = 'badge';
                badge.textContent = '🥗 Végétarien';
                badges.appendChild(badge);
            }
            
            if (recette.saisons && recette.saisons.length > 0) {
                recette.saisons.forEach(saison => {
                    const badge = document.createElement('span');
                    badge.className = 'badge';
                    badge.textContent = `${obtenirEmoji(saison)} ${formatterNom(saison)}`;
                    badges.appendChild(badge);
                });
            }
            
            card.appendChild(badges);
            section.appendChild(card);
        });
        
        conteneur.appendChild(section);
    };
    
    afficherGroupe('Entrées', entrees, '🥗');
    afficherGroupe('Plats', plats, '🍽️');
    afficherGroupe('Desserts', desserts, '🍰');
}

/**
 * Affiche les détails d'une recette
 * @param {string} nomRecette - Nom de la recette
 */
async function afficherDetailsRecette(nomRecette) {
    try {
        const response = await apiGet(`/recettes/details?nom=${nomRecette}`);
        
        if (response.success && response.data) {
            const recette = response.data;
            alert(`Détails de ${formatterNom(recette.nom)}:\n\nDescription: ${recette.description || 'N/A'}\nType: ${recette.type}\nVégétarien: ${recette.vegetarien ? 'Oui' : 'Non'}`);
        }
    } catch (error) {
        afficherNotification('Erreur lors du chargement des détails', 'error');
        gererErreur(error);
    }
}

// -----------------------------------------------------------------------------
// TRACE DU RAISONNEMENT
// -----------------------------------------------------------------------------

/**
 * Affiche la trace du raisonnement dans l'onglet Trace
 */
function afficherTrace() {
    const conteneur = document.getElementById('trace-container');
    conteneur.innerHTML = '';
    
    if (state.trace.length === 0) {
        conteneur.innerHTML = '<p class="info">Aucune trace disponible. Lancez d\'abord une recherche de recettes.</p>';
        return;
    }
    
    const titre = document.createElement('h3');
    titre.textContent = `Trace du raisonnement (${state.trace.length} étape(s))`;
    conteneur.appendChild(titre);
    
    state.trace.forEach(etape => {
        const item = document.createElement('div');
        item.className = 'trace-item';
        
        const numero = document.createElement('span');
        numero.className = 'trace-numero';
        numero.textContent = `Étape ${etape.etape}`;
        
        const regle = document.createElement('span');
        regle.className = 'trace-regle';
        regle.textContent = `Règle: ${formatterNom(etape.regle)}`;
        
        const fait = document.createElement('span');
        fait.className = 'trace-fait';
        fait.textContent = `→ ${formatterNom(etape.fait)}`;
        
        item.appendChild(numero);
        item.appendChild(regle);
        item.appendChild(fait);
        conteneur.appendChild(item);
    });
}

// -----------------------------------------------------------------------------
// NOTIFICATIONS
// -----------------------------------------------------------------------------

/**
 * Affiche une notification à l'utilisateur
 * @param {string} message - Message à afficher
 * @param {string} type - Type de notification ('success', 'error', 'warning')
 */
function afficherNotification(message, type = 'success') {
    const notification = document.createElement('div');
    notification.className = `notification ${type}`;
    notification.textContent = message;
    
    document.body.appendChild(notification);
    
    // Style basique
    notification.style.position = 'fixed';
    notification.style.bottom = '20px';
    notification.style.right = '20px';
    notification.style.padding = '15px 20px';
    notification.style.borderRadius = '5px';
    notification.style.color = 'white';
    notification.style.zIndex = '1000';
    notification.style.boxShadow = '0 2px 10px rgba(0,0,0,0.2)';
    
    if (type === 'success') notification.style.backgroundColor = '#4CAF50';
    else if (type === 'error') notification.style.backgroundColor = '#f44336';
    else if (type === 'warning') notification.style.backgroundColor = '#ff9800';
    
    setTimeout(() => {
        notification.remove();
    }, 3000);
}

// -----------------------------------------------------------------------------
// UTILITAIRES
// -----------------------------------------------------------------------------

/**
 * Formatte un nom de symbole Lisp en texte lisible
 * @param {string} nom - Nom du symbole (ex: 'pate_brisee')
 * @returns {string} Texte formaté (ex: 'Pâte Brisée')
 */
function formatterNom(nom) {
    if (!nom) return '';
    return nom
        .replace(/_/g, ' ')
        .split(' ')
        .map(mot => mot.charAt(0).toUpperCase() + mot.slice(1).toLowerCase())
        .join(' ');
}

/**
 * Convertit une valeur en icône emoji
 * @param {string} cle - Clé à convertir (ex: 'vegetarien', 'printemps')
 * @returns {string} Emoji correspondant
 */
function obtenirEmoji(cle) {
    const emojis = {
        'vegetarien': '🥗',
        'printemps': '🌸',
        'ete': '☀️',
        'automne': '🍂',
        'hiver': '❄️',
        'entree': '🥗',
        'plat': '🍽️',
        'dessert': '🍰'
    };
    
    return emojis[cle] || '';
}

// -----------------------------------------------------------------------------
// GESTION DES ERREURS
// -----------------------------------------------------------------------------

/**
 * Gère les erreurs d'API de manière centralisée
 * @param {Error} error - Erreur capturée
 */
function gererErreur(error) {
    console.error('Erreur:', error);
    
    if (error.message.includes('Failed to fetch') || error.message.includes('NetworkError')) {
        afficherNotification('Serveur non accessible. Vérifiez qu\'il est démarré.', 'error');
    } else {
        afficherNotification(`Erreur: ${error.message}`, 'error');
    }
}

// -----------------------------------------------------------------------------
// EXPORT POUR TESTS (optionnel)
// -----------------------------------------------------------------------------
// Si vous voulez tester les fonctions individuellement
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        afficherOnglet,
        chargerIngredients,
        chargerMateriel,
        lancerRecherche,
        formatterNom,
        obtenirEmoji
    };
}
