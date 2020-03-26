import Control.Monad.Reader
import System.Cron.Schedule

import qualified DBAdapter as DB
import App
import Config
import Poll

main :: IO ()
main = do
  conf <- config
  _ <- execSchedule $
        addJob (runReaderT pollTargets  conf) $ pollSchedule conf
  runReaderT DB.initDB conf
  runApp conf