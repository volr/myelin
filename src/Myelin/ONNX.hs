{-# LANGUAGE TemplateHaskell #-}
module Myelin.ONNX where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad

data STyp = 
      UInt8
    | UInt16
    | UInt32
    | Int8
    | Int16
    | Int32
    | Float16
    | Float32
    | Float64
    | Bool
    deriving (Show, Read)

data Tensor a = Tensor {
    scalar_type :: STyp,
    dimensions :: [Int]
} deriving (Show)

data Padding = SameUpper | SameLower | Valid deriving (Show, Read)


data Label a = Label {
  label :: Int,
  content :: a
} deriving (Show)

data Op a =
    Abs { 
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | Add { 
        broadcast :: Bool,
        axis :: Int,
        a :: Label (Tensor a),
        b :: Label (Tensor a),
        c :: Label (Tensor a)   
    }
    | And {
        broadcast :: Bool,
        axis :: Int,
        a :: Label (Tensor a), -- Tensor Bool
        b :: Label (Tensor a), -- Tensor Bool 
        c :: Label (Tensor a) -- Tensor Bool
    }
    | ArgMax {
        axis :: Int,
        keepdims :: Bool,
        dat :: Label (Tensor a),
        reduced :: Label (Tensor a)
    }
    | ArgMin {
        axis :: Int,
        keepdims :: Bool,
        dat :: Label (Tensor a),
        reduced :: Label (Tensor a)
    }
    | AveragePool {
        auto_pad :: Padding,
        kernel_shape :: [Int],
        pads :: Maybe [Int],
        strides :: Maybe [Int],
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | BatchNormalization {
        epsilon :: Float,
        is_test :: Bool,
        momentum :: Float,
        spatial :: Bool,
        x :: Label (Tensor a),
        scale :: Label (Tensor a),
        b :: Label (Tensor a),
        mean :: Label (Tensor a),
        var :: Label (Tensor a),
        y :: Label (Tensor a),
        saved_mean :: Label (Tensor a),
        saved_var :: Label (Tensor a)
    }
    | Cast {
        to :: STyp,
        input :: Label (Tensor a),
        ouput :: Label (Tensor a)
    }
    | Ceil {
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | Clip {
        max :: Float,
        min :: Float,
        input :: Label (Tensor a),
        out :: Label (Tensor a)
    }
    | Concat {
        axis :: Int,
        inputs :: [Label (Tensor a)],
        concat_result :: Label (Tensor a)
    }
    | Constant {
        value :: Tensor a        
    }
    | Conv {
        auto_pad :: Padding,
        dilations :: Maybe [Int],
        group :: Maybe Int,
        pads :: Maybe [Int],
        strides :: Maybe [Int],
        x :: Label (Tensor a),
        w :: Label (Tensor a),
        b :: Label (Tensor a),
        y :: Label (Tensor a)        
    }
    | ConvTranspose {
        auto_pad :: Padding,
        dilations :: Maybe [Int],
        group :: Maybe Int,
        kernel_shape :: [Int],
        output_padding :: [Int],
        output_shape :: [Int],        
        pads :: Maybe [Int],
        strides :: Maybe [Int],
        x :: Label (Tensor a),
        w :: Label (Tensor a),
        b :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | DepthToSpace {
        blocksize :: Int,
        input :: Label (Tensor a),
        output :: Label (Tensor a)
    }
    | Div {
        broadcast :: Bool, 
        axis :: Int,
        a :: Label (Tensor a),
        b :: Label (Tensor a),
        c :: Label (Tensor a) 
    }
    | Dropout {
        is_test :: Bool,
        ratio :: Float,
        dat :: Label (Tensor a),
        output :: Label (Tensor a),
        mask :: Label (Tensor a)
    }
    | Elu {
        alpha :: Float,
        x :: Label (Tensor a),
        y :: Label (Tensor a)        
    }
    | Equal {
        broadcast :: Bool, 
        axis :: Int,
        a :: Label (Tensor a),
        b :: Label (Tensor a),
        c :: Label (Tensor a)
    }
    | Exp {
        input :: Label (Tensor a),
        output :: Label (Tensor a)
    }
    | Flatten {
        broadcast :: Bool, 
        axis :: Int,
        input :: Label (Tensor a),
        output :: Label (Tensor a)
    }
    | Floor {
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | GRU -- TODO
    | Gather -- TODO
    | Gemm -- TODO 
    | GlobalAveragePool {
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | GlobalLpPool {
        norm_p :: Int,
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | GlobalMaxPool {
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | Greater {
        broadcast :: Bool, 
        axis :: Int,
        a :: Label (Tensor a),
        b :: Label (Tensor a),
        c :: Label (Tensor a)
    }
    | HardSigmoid {
        alpha :: Float,
        beta :: Float,
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | Hardmax {
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | InstanceNormalization -- TODO
    | LRN -- TODO
    | LSTM {
        x :: Label (Tensor a),
        w :: Label (Tensor a),
        r :: Label (Tensor a),
        b :: Label (Tensor a),
        sequence_lens :: Maybe (Label (Tensor a)),
        initial_h :: Maybe (Label (Tensor a)),
        initial_c :: Maybe (Label (Tensor a)),
        peephole :: Maybe (Label (Tensor a)),
        -- Ouputs 
        y :: Label (Tensor a),
        y_h :: Label (Tensor a),
        y_c :: Label (Tensor a)
    }
    | LeakyRelu {
        alpha :: Float,
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | Less  {
        broadcast :: Bool, 
        axis :: Int,
        a :: Label (Tensor a),
        b :: Label (Tensor a),
        c :: Label (Tensor a)
    }
    | Log {
        input :: Label (Tensor a),
        output :: Label (Tensor a)
    }
    | LogSoftmax -- TODO
    | LpNormalization {
        axis :: Int,
        p :: Int,
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | LpPool
    | MatMul {
        a :: Label (Tensor a),
        b :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | Max
    | MaxPool
    | MaxRoiPool
    | Mean
    | Min
    | Mul
    | Neg
    | Not
    | Or
    | PRelu
    | Pad
    | Pow
    | RNN
    | RandomNormal
    | RandomNormalLike
    | RandomUniform
    | RandomUniformLike
    | Reciprocal
    | ReduceL1
    | ReduceL2
    | ReduceLogSum
    | ReduceLogSumExp
    | ReduceMax
    | ReduceMean
    | ReduceMin
    | ReduceProd
    | ReduceSum
    | ReduceSumSquare
    | Relu {
        x :: Label (Tensor a),
        y :: Label (Tensor a)
    }
    | Reshape
    | Selu
    | Shape
    | Sigmoid
    | Size
    | Slice
    | Softmax
    | Softplus
    | Softsign
    | SpaceToDepth
    | Split
    | Sqrt
    | Squeeze
    | Sub
    | Sum
    | Tanh
    | Tile
    | TopK
    | Transpose
    | Unsqueeze
    | Xor
    deriving (Show)

data BlockState = BlockState {
  _nextLabel :: Int,
  _tensors :: [Label (Tensor STyp)],
  _block :: [Op STyp]
  } deriving (Show)

makeLenses ''BlockState

initialBlockState = BlockState 0 [] []

type Block a = StateT BlockState IO a

tensor :: STyp -> [Int] -> Block (Label (Tensor STyp))
tensor typ dim = do
  let t = Tensor typ dim
  l <- use nextLabel
  nextLabel += 1
  tensors <>= [Label l t]
  return $ Label l t

abs_ :: Label (Tensor STyp) -> Block (Label (Tensor STyp))
abs_ x = do
  y <- tensor (scalar_type $ content $ x) (dimensions $ content $ x)
  block <>= [Abs x y]
  return y

add_ :: Label (Tensor STyp) -> Label (Tensor STyp) -> Bool -> Int ->  Block (Label (Tensor STyp))
add_ a b br ax = do
  c <- tensor (scalar_type $ content $ a) (dimensions $ content $ a)
  block <>= [Add br ax a b c]
  return c
  
network :: Block (Label (Tensor STyp))
network = do
  a <- tensor UInt8 [4,5]
  b <- abs_ a
  add_ a b False 0


someFunc :: IO ()
someFunc = do
  b <- execStateT network initialBlockState
  print b
  return ()
