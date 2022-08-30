{***************************************************************************}
{ TPlanner SMS alarm interface component for Oxygen SMS component           }
{ for Delphi 3.0,4.0,5.0,6.0 & C++Builder 3.0,4.0,5.0,6.0                   }
{ version 1.1 - rel. July 2002                                              }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2002                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit oxygensmsalarm;

interface

uses
  Classes, Planner, SMSComp, Dialogs, Windows;

type
  TOxygenSMSAlarm = class(TPlannerAlarmHandler)
  private
    FAutoDelMsg: Boolean;
    FAutoDelRpt: Boolean;
    FTag: Integer;
    FConnMode: Integer;
    FModel: Integer;
    FComNumber: Integer;
    FOxygenSMS: TOxygenSMS;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    function HandleAlarm(Address,Message:string; Tag, ID: Integer;
      Item: TPlannerItem): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoDeleteMessages: Boolean read FAutoDelMsg write FAutoDelMsg;
    property AutoDeleteReports: Boolean read FAutoDelRpt write FAutoDelRpt;
    property ComNumber: Integer read FComNumber write FComNumber;
    property ConnectionMode: Integer read FConnMode write FConnMode;
    property Model: Integer read FModel write FModel;
    property OxygenSMS: TOxygenSMS read FOxygenSMS write FOxygenSMS;
    property Tag: Integer read FTag write FTag;
  end;

procedure Register;


implementation

{ TOxygenSMSAlarm }

constructor TOxygenSMSAlarm.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TOxygenSMSAlarm.Destroy;
begin
  inherited;
end;

function TOxygenSMSAlarm.HandleAlarm(Address, Message: string; Tag,
  ID: Integer; Item: TPlannerItem): Boolean;
begin
  Result := False;
  if not Assigned(FOxygenSMS) then Exit;
  FOxygenSMS.AutoDeleteMessages := FAutoDelMsg;
  FOxygenSMS.AutoDeleteReports := FAutoDelRpt;

  FOxygenSMS.ComNumber := FComNumber;
  FOxygenSMS.ConnectionMode := FConnMode;
  FOxygenSMS.Model := FModel;

  if FOxygenSMS.Open then
  begin
    Result := FOxygenSMS.SendSMSMessage(item.alarm.Address,item.alarm.message,0,False,False,Nil);
    FOxygenSMS.Close;
  end;
end;

procedure TOxygenSMSAlarm.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FOxygenSMS) then
    FOxygenSMS := nil;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('TMS Planner',[TOxygenSMSAlarm]);
end;

end.
