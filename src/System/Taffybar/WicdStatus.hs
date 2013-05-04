{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.WicdStatus (networkStatusNew) where

import DBus
import DBus.Client
import Data.Word
import Data.Maybe
import Graphics.UI.Gtk hiding (Variant)


data WicdNetworkInfo = WicdNotConnected
                     | WicdConnectingWired
                     | WicdConnectingWireless { infoSsid :: String }
                     | WicdWireless { infoIp :: String
                                    , infoSsid :: String
                                    , infoStrength :: Int
                                    , infoSpeed :: String
                                    }
                     | WicdWired { infoIp :: String }
                     | WicdSuspended

instance Show WicdNetworkInfo where
  show WicdNotConnected = "not connected"
  show WicdConnectingWired = "connecting to wired network..."
  show (WicdConnectingWireless ssid) = "connecting to " ++ ssid ++ "..."
  show (WicdWired ip) = "LAN:" ++ ip
  show (WicdWireless ip ssid strength _) = ssid ++ "(" ++ show strength ++ "%):" ++ ip
  show WicdSuspended = "wicd suspended"


convertStatusMsg :: [Variant] -> WicdNetworkInfo
convertStatusMsg b = 
  let [tid', mdata'] = b
      Just tid = fromVariant tid' :: Maybe Word32
      Just mdata'' = fromVariant mdata' :: Maybe [Variant]
      mdata = map (fromJust . fromVariant) mdata'' :: [String] in
  case tid of
    0 -> WicdNotConnected
    1 -> case mdata of ["wireless", ssid] -> WicdConnectingWireless ssid
                       _                  -> WicdConnectingWired
    2 -> let [ip, ssid, strength, _, speed] = mdata in
         WicdWireless ip ssid (read strength) speed
    3 -> WicdWired (head mdata)
    4 -> WicdSuspended

networkStatusNew :: IO Widget
networkStatusNew = do
    l <- labelNew Nothing
    labelSetMarkup l ""
    _ <- on l realize $ do ss <- connectSystem
                           listen ss rule (callback l)
    widgetShowAll l
    return (toWidget l)
  where
    rule = matchAny { matchPath = Just "/org/wicd/daemon"
                    , matchMember = Just "StatusChanged" }
    callback lbl sig = postGUIAsync $ labelSetMarkup lbl $ show $
                       convertStatusMsg (signalBody sig)
