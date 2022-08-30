object ISAPI_Server: TISAPI_Server
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 405
  Top = 172
  Height = 150
  Width = 215
  object Server: TRtcISAPIServer
    FixupRequest.RemovePrefix = True
    FixupRequest.DecodeFileName = True
    Left = 40
    Top = 15
  end
end
