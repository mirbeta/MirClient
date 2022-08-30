{
  @html(<b>)
  Message-based communication Components Registration
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  Message-based communication components are being
  registered to Delphi component palette.
  
  @exclude
}
unit rtcRegisterMsg;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
{$IFNDEF RTC_NOINTF}
  {$IFNDEF FPC}
    {$IFDEF IDE_6up}
      DesignIntf,
    {$ELSE}
      TypInfo, Consts,
      DsgnIntf,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  Classes,

  rtcTypes,

{$IFNDEF RTC_NOINTF}
  rtcEditors,
  rtcTransports,
{$ENDIF}

  rtcMsgSrv,
  rtcMsgCli;

{$IFNDEF RTC_NOINTF}
  {$IFNDEF FPC}
  type
    TRtcMessageReceiverInterfacedComponentProperty = class(TRtcInterfacedComponentProperty)
      public
        function GetIID: TGUID; override;
      end;

  function TRtcMessageReceiverInterfacedComponentProperty.GetIID: TGUID;
    begin
    Result := IRTCMessageReceiverGUID;
    end;
  {$ENDIF}
{$ENDIF}

procedure Register;
  begin
  RegisterComponents('RTC Server',[TRtcMessageServer]);

  RegisterComponents('RTC Client',[TRtcMessageClient]);

{$IFNDEF RTC_NOINTF}
  {$IFNDEF FPC}
  RegisterPropertyEditor(TComponent.ClassInfo,
                         TRtcMessageClient, 'Server',
                         TRtcMessageReceiverInterfacedComponentProperty);
  {$ENDIF}
{$ENDIF}
  end;

initialization
end.
