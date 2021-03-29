{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCube.PipelineSchemaUtil where

import           Control.Monad.Writer      (MonadWriter (tell), Writer,
                                            execWriter)
import qualified Data.Map                  as Map
import           LambdaCube.IR             (FetchPrimitive, InputType)
import           LambdaCube.PipelineSchema (ObjectArraySchema (ObjectArraySchema),
                                            PipelineSchema (PipelineSchema),
                                            StreamType)

(@:) :: MonadWriter [(a, b)] m => a -> b -> m ()
a @: b = tell [(a,b)]

defObjectArray :: MonadWriter PipelineSchema m => String -> FetchPrimitive -> Writer [(String, StreamType)] a -> m ()
defObjectArray n p m = mapM_ tell [PipelineSchema (Map.singleton n $ ObjectArraySchema p $ Map.singleton a t) mempty | (a,t) <- execWriter m]

defUniforms :: MonadWriter PipelineSchema m => Writer [(String, InputType)] a -> m ()
defUniforms m = tell $ PipelineSchema mempty $ Map.fromList $ execWriter m

makeSchema :: Writer PipelineSchema a -> PipelineSchema
makeSchema a = execWriter a :: PipelineSchema

unionObjectArraySchema :: ObjectArraySchema -> ObjectArraySchema -> ObjectArraySchema
unionObjectArraySchema (ObjectArraySchema a1 b1) (ObjectArraySchema a2 b2) =
  ObjectArraySchema (if a1 == a2 then a1 else error $ "object array schema primitive mismatch " ++ show (a1,a2))
                    (Map.unionWith (\a b -> if a == b then a else error $ "object array schema attribute type mismatch " ++ show (a,b)) b1 b2)

instance Monoid PipelineSchema where
  mempty = PipelineSchema mempty mempty
#if !MIN_VERSION_base(4,11,0)
  mappend (PipelineSchema a1 b1) (PipelineSchema a2 b2) =
    PipelineSchema (Map.unionWith unionObjectArraySchema a1 a2) (Map.unionWith (\a b -> if a == b then a else error $ "schema type mismatch " ++ show (a,b)) b1 b2)
#else
instance Semigroup PipelineSchema where
  (<>)    (PipelineSchema a1 b1) (PipelineSchema a2 b2) =
    PipelineSchema (Map.unionWith unionObjectArraySchema a1 a2) (Map.unionWith (\a b -> if a == b then a else error $ "schema type mismatch " ++ show (a,b)) b1 b2)
#endif
