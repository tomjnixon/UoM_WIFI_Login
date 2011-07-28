import Network.Shpider
import Network.Info
import List
import Data.Maybe
import System.Environment
import System.Exit

main_url = "https://wireless.its.manchester.ac.uk/upload/custom/Aruba-Captive-Portal/index.html?cmd=login&essid=UoM_WIFI"
redirect_url = "http://www.google.co.uk"

data Credentials = Credentials {user:: String, password:: String} deriving (Read, Show)

auth credfile interface = do
	Credentials {user=user, password=password} <- liftM read $ readFile credfile
	NetworkInterface {ipv4=ip, mac=mac} <- liftM (fromJust . find ((==interface).name)) getNetworkInterfaces
	let params = [ ("ip", show ip)
	             , ("mac", show mac)
	             , ("url", redirect_url)
	             ]
	let url = exportURL $ foldl add_param (fromJust $ importURL main_url) params
	runShpider $ do
		download url
		(form:_) <- currentForms;
		sendForm $ fillOutForm form
			[ ("user", user)
			, ("password", password)
			]

usage = do
	progName <- getProgName
	putStrLn "Usage: "
	putStrLn $ progName ++ " cred_file interface_name"
	exitFailure

main = do
	args <- getArgs
	case args of
		[credfile, interface] -> do
			auth credfile interface
			exitSuccess
		_ -> usage
