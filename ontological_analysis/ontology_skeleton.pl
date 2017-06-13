%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ontology_skeleton.pl:
%
%                   Onthology skleleton for onthological
%                disambiguation of Natural Logics Grammar.
%
%
% This file aims at defining a top ontology for concepts related to 
% human insulin production based on BFO. Domain specific concepts 
% are then created based on the insulin article considered as a 
% reference. Some relations in the file below have been commented in 
% order to simplify the model and thus, to increase the efficiency of 
% the parsing.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Ontology skeleton inspired from Basic Formal Ontology (BFO).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_subset_of(continuant, entity).
is_subset_of(occurent, entity).

is_subset_of(independent_continuant, continuant).
is_subset_of(material, independent_continuant).
is_subset_of(immaterial, independent_continuant).

is_subset_of(gen_dependent_continuant, continuant).
is_subset_of(information, gen_dependent_continuant).
% is_subset_of(gene_sequence, gen_dependent_continuant).

is_subset_of(spe_dependent_continuant, continuant).
is_subset_of(quality, spe_dependent_continuant).
is_subset_of(quantity,quality).

% is_subset_of(function, spe_dependent_continuant).
% is_subset_of(disposition, function).
% is_subset_of(regulation, disposition).

is_subset_of(process, occurent).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Domain specific ontology for human insulin production.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% is_subset_of(immaterial, independent_continuant).
% is_subset_of(cognitive, immaterial).
% is_subset_of(idea, cognitive).

is_subset_of(animate, material).
% is_subset_of(person, animate).
% is_subset_of(doctor, person).

is_subset_of(inanimate, material).
is_subset_of(countable, inanimate).
is_subset_of(mass, inanimate).
% is_subset_of(artifact, countable).
is_subset_of(body_part, countable).
is_subset_of(organ, body_part).
is_subset_of(cell, body_part).
is_subset_of(body_fluid, mass).
is_subset_of(body_secretion, body_fluid).
is_subset_of(protein, body_secretion).

is_subset_of(state, occurent).
% is_subset_of(presence, state).
% is_subset_of(lack, state).

% is_subset_of(event, occurent).
% is_subset_of(action, event).
% is_subset_of(treatment, action).

is_subset_of(creative, process).
is_subset_of(non_creative, process).
is_subset_of(creation, creative).
% is_subset_of(synthesis, creation).
is_subset_of(transformation, creative).
% is_subset_of(conversion, transformation).
is_subset_of(enhancement, non_creative).
% is_subset_of(inhibition, enhancement).
% is_subset_of(promotion, enhancement).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%