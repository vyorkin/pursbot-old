# Pursbot

PureScript telegram bot.

## Interacting

Searching:

```
/search (a -> b) -> f a -> f b

liftA1 :: forall f a b. Applicative f => (a -> b) -> f a -> f b
in Control.Applicative
of prelude v4.1.0
https://pursuit.purescript.org/packages/purescript-prelude/4.1.0/docs/Control.Applicative#v:liftA1

liftM1 :: forall m a b. Monad m => (a -> b) -> m a -> m b
in Control.Monad
of prelude v4.1.0
https://pursuit.purescript.org/packages/purescript-prelude/4.1.0/docs/Control.Monad#v:liftM1

map :: forall a b f. Functor f => (a -> b) -> f a -> f b
in Data.Functor
of prelude v4.1.0
https://pursuit.purescript.org/packages/purescript-prelude/4.1.0/docs/Data.Functor#v:map

mapDefault :: forall i f a b. FunctorWithIndex i f => (a -> b) -> f a -> f b"
in Data.FunctorWithIndex
of foldable-traversable v4.0.0
https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.0.0/docs/Data.FunctorWithIndex#v:mapDefault

...
```

Asking for a function type: TODO

Evaluation: TODO

## Development

1. Get your own token by using the `@BotFather` as described [here](https://core.telegram.org/bots#botfather).
2. Then create a config file and change it accordingly:
```
cp config.example.json config.json
```
3. Initiate a conversation with your bot

## Roadmap

Currently **pursbot** only supports search queries (by hitting the `Pursuit` API).
List of features to be implemented (almost everything from the [lambdabot wiki page](https://wiki.haskell.org/Lambdabot)):

* Searching with Pursuit: `query [a] -> [a]`
* Asking for a functions type, like `:t` in `purs repl`, using either that or `ty`, `type`
* PureScript evaluation: `eval take 5 (1..10) ==> [1, 2, 3, 4, 5]`
* Defining persistent bindings for evaluation: `let x = 4 sets x to 4`. `undef` deletes old definitions.
* Pointfree and pointfull refactorings.
* Convert do notation to bind notation.
* **Djinn** takes a type signature and constructs a function satisfying it: `djinn (a, b) -> c -> (b, c) ==> f (_, a) b = (a, b)`
* What type does a monad transformer wrap? `unmtl StateT s Effect a ==> s -> Effect (a, s)`
