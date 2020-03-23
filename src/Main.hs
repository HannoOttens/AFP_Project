
import System.Cron.Schedule
import Control.Monad.Reader

import App
import qualified DBAdapter as DB
import Config
import Poll

main :: IO ()
main = do
  conf <- config
  _ <- execSchedule $
        addJob (runReaderT pollTargets  conf) $ pollSchedule conf
  runReaderT DB.initDB conf
  runApp conf