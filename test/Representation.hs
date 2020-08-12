{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Representation ( representationTests ) where

import           LLVM.AST                  hiding (nsw, nuw)
import           LLVM.Prelude              hiding (void)

import           InternalIR.SimplePath
import           Test.Tasty.HUnit

import           TestingDSL
import           Utils

default (Integer, ShortByteString)

representationTests :: BenchTest
representationTests = benchTestGroup "Representation" [
    benchTestGroup "phisToPar" [
      benchTestCase "one phi"   $ one_phi_test
    , benchTestCase "two phi"   $ two_phi_test
    , benchTestCase "three phi" $ three_phi_test
    ]
  ]

one_phi_test =
  phisToPar [ "x" := phi i32 [("x0", "entry"), ("x1", "one"), ("x2", "two"), ("x3", "three")] ]
    @=?  ParEqs [["x" := bitcast i32 "x0" i32]
                ,["x" := bitcast i32 "x1" i32]
                ,["x" := bitcast i32 "x2" i32]
                ,["x" := bitcast i32 "x3" i32] ]

two_phi_test =
  phisToPar [ "x" := phi i32 [("x0", "entry"), ("x1", "one"), ("x2", "two"), ("x3", "three")]
            , "y" := phi i32 [("y1", "one"), ("y2", "two"), ("y0", "entry"), ("y3", "three")] ]
    @=?  ParEqs [["x" := bitcast i32 "x0" i32, "y" := bitcast i32 "y0" i32]
                ,["x" := bitcast i32 "x1" i32, "y" := bitcast i32 "y1" i32]
                ,["x" := bitcast i32 "x2" i32, "y" := bitcast i32 "y2" i32]
                ,["x" := bitcast i32 "x3" i32, "y" := bitcast i32 "y3" i32] ]

three_phi_test =
  phisToPar ["x" := phi i32 [("x0", "entry"), ("x1", "one"), ("x2", "two"), ("x3", "three")]
            ,"y" := phi i32 [("y1", "one"), ("y2", "two"), ("y0", "entry"), ("y3", "three")]
            ,"z" := phi i32 [("z2", "two"), ("z1", "one"), ("z0", "entry"), ("z3", "three")] ]
    @=?  ParEqs [["x" := bitcast i32 "x0" i32, "y" := bitcast i32 "y0" i32, "z" := bitcast i32 "z0" i32]
                ,["x" := bitcast i32 "x1" i32, "y" := bitcast i32 "y1" i32, "z" := bitcast i32 "z1" i32]
                ,["x" := bitcast i32 "x2" i32, "y" := bitcast i32 "y2" i32, "z" := bitcast i32 "z2" i32]
                ,["x" := bitcast i32 "x3" i32, "y" := bitcast i32 "y3" i32, "z" := bitcast i32 "z3" i32] ]
