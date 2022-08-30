object IcsTcpSvc: TIcsTcpSvc
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'ICS Tcp Service'
  StartType = stManual
  OnExecute = ServiceExecute
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 161
  Width = 270
end
