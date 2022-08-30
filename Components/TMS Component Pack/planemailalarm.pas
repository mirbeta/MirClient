{***************************************************************************}
{ TPlanner Email alarm interface component for SakEmail SMTP component      }
{ for Delphi 3.0,4.0,5.0,6.0 & C++Builder 3.0,4.0,5.0                       }
{ version 1.0 - rel. July 2001                                              }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001                                               }
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

unit PlanEmailAlarm;

interface

uses
  Classes, Planner, SakMsg, SakSMTP;

type

  TAlarmEmail = class(TPlannerAlarmHandler)
  private
    FSakMsg: TSakMsg;
    FSakSMTP: TSakSMTP;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    function HandleAlarm(Address,Message:string; Tag, ID: Integer;
      Item: TPlannerItem): Boolean; override;
  published
    property SakMessage: TSakMsg read FSakMsg write FSakMsg;
    property SakSMTP: TSakSMTP read FSakSMTP write FSakSMTP;
  end;

procedure Register;


implementation


{ TAlarmEmail }

function TAlarmEmail.HandleAlarm(Address, Message: string; Tag,
  ID: Integer; Item: TPlannerItem): Boolean;
begin
  Result := False;
  if not Assigned(FSakMsg) then
    Exit;
  if not Assigned(FSakSMTP) then
    Exit;

  if Address = '' then
    Exit;

  FSakMsg.SendTo := Address;
  FSakMsg.Subject := Message;
  FSakMsg.Text.Text := Item.StrippedItemText;

  FSakSMTP.Connect;
  if not FSakSMTP.SmtpError then
  begin
    Result := SakSMTP.SendMessage(FSakMsg);
  end;
  FSakSMTP.Quit;
end;

procedure TAlarmEmail.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FSakMsg) then
    FSakMsg := nil;

  if (AOperation = opRemove) and (AComponent = FSakSMTP) then
    FSakSMTP := nil;

  inherited;
end;

procedure Register;
begin
  RegisterComponents('TMS Planner',[TAlarmEmail]);
end;



end.
