GastroGnome
===========

GastroGnome is a domain-specific language embedded in Haskell which is
designed to allow chefs of all experience levels to express recipes in a
clear and unambiguous way, which can also be easily interpreted by a machine to
create a platform on which a variety of exciting tools can be built. We have
prototyped some simple tools to demonstrate the value of the language,
including a "kitchen assistant" which can translate a recipe into a sequence of
prompts to the chef while they are cooking, and a tool to generate a shopping
list based on a recipe and a list of available ingredients in the pantry.

To get a flavor for the language, here is a recipe for waffles written in
GastroGnome:

    Dry Ingredients:
      MIX
        2 cup Flour
        1 tablespoon Sugar
        4 teaspoon Baking powder
        1/4 tsp Salt

    Batter:
      BEAT in large bowl until “smooth”
        BEAT until fluffy
          2 Egg
        Dry Ingredients
        1.75 cup Milk
        1/2 cup Vegetable Oil
        1/2 tsp Vanilla

    Prepared Waffle Iron:
      SPRAY with Cooking Spray
        PREHEAT for 15 min
          Waffle Iron

    Waffles:
      COOK at 300 F in Prepared Waffle Iron until “golden brown”
        Batter
