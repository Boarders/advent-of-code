{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
module Common where


import Data.Primitive.ByteArray
import System.IO
import Control.Exception
import Data.Primitive.Types
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.ByteString.Internal
import Foreign.Storable hiding (sizeOf)

readFileBA :: FilePath -> IO ByteArray
readFileBA f = 
  withBinaryFile f ReadMode $ \h -> do
      -- hFileSize fails if file is not regular file (like
      -- /dev/null). Catch exception and try reading anyway.
      filesz <- catch (hFileSize h) useZeroIfNotRegularFile
      let readsz = (fromIntegral filesz `max` 0) + 1
      hGetContentsSizeHint h readsz (readsz `max` 255)
      -- Our initial size is one bigger than the file size so that in the
      -- typical case we will read the whole file in one go and not have
      -- to allocate any more chunks. We'll still do the right thing if the
      -- file size is 0 or is changed before we do the read.
  where
    useZeroIfNotRegularFile :: IOException -> IO Integer
    useZeroIfNotRegularFile _ = return 0


hGetContentsSizeHint :: Handle
                     -> Int -- ^ first read size
                     -> Int -- ^ initial buffer size increment
                     -> IO ByteArray
hGetContentsSizeHint hnd =
    readChunks []
  where
    readChunks chunks sz sz' = do
      ba        <- newPinnedByteArray (sz - 1)
      let ptrBa = mutableByteArrayContents ba
      readcount <- hGetBuf hnd ptrBa sz
      chunk <- unsafeFreezeByteArray ba
      -- We rely on the hGetBuf behaviour (not hGetBufSome) where it reads up
      -- to the size we ask for, or EOF. So short reads indicate EOF.
      if readcount < sz && sz > 0
        then return $! mconcat (reverse (chunk : chunks))
        else readChunks (chunk : chunks) sz' ((sz+sz') `min` 32752)
             -- we grow the buffer sizes, but not too huge
             -- we concatenate in the end anyway


foldlBA :: forall a b. (Prim a) => (b -> a -> b) -> b -> ByteArray -> b
foldlBA f z arr = go 0 z
  where
    go i acc
      | i < maxI  = go (i + 1) (f acc (indexByteArray arr i)) 
      | otherwise = acc
    maxI = sizeofByteArray arr `quot` sizeOf (undefined :: a)

--{-# INLINE myfoldl' #-}
myfoldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
myfoldl' f v (PS fp off len) =
      accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
        go v (p `plusPtr` off) (p `plusPtr` (off+len))
    where
      -- tail recursive; traverses array left to right
      go !z !p !q | p == q    = return z
                  | otherwise = do x <- peek p
                                   go (f z x) (p `plusPtr` 1) q

solutions :: (Show a, Show b) => Int -> a -> b -> String
solutions n s1 s2 = unlines $
  ["~~~ Day " <> show n <> "~~~"
  ,""
  ,"solution 1: " <> (show s1)
  ,"solution 2: " <> (show s2)
  ]



