{**************************************************************************}
{ TDBAdvSpinEdit component                                                 }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{           copyright © 2000 - 2014                                        }
{           Email : info@tmssoftware.com                                   }
{           Web : http://www.tmssoftware.com                               }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit DBAdvSp;

{$I TMSDEFS.INC}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AdvSpin, DB, DBCtrls;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvSpinEdit = class(TAdvSpinEdit)
  private
    { Private declarations }  
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(const Value: Boolean);
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure WMChar(var Message: TWMKeyDown); message WM_CHAR;
    procedure CMExit(var Message: TWMNoParams); message CM_EXIT;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpClick (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;


implementation

{ TDBAdvSpinEdit }

procedure TDBAdvSpinEdit.ActiveChange(Sender: TObject);
begin
  if Assigned(FDatalink.DataSet) then
  begin
    if not FDataLink.DataSet.Active then
      Text := '';
  end;
end;

procedure TDBAdvSpinEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TDBAdvSpinEdit.CMExit(var Message: TWMNoParams);
begin
  inherited;
  if not FDataLink.ReadOnly then
  begin
   try
      FDataLink.UpdateRecord;                          { tell data link to update database }
   except
      on Exception do SetFocus;                      { if it failed, don't let focus leave }
   end;
  end;
end;

procedure TDBAdvSpinEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

constructor TDBAdvSpinEdit.Create(aOwner: TComponent);
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  FDataLink.OnActiveChange := ActiveChange;
end;

procedure TDBAdvSpinEdit.DataChange(Sender: TObject);
begin
  try
    if Assigned(FDataLink.Field) then
  	  if FDataLink.Field.IsNull then
	      Text := ''
      else
        case SpinType of
        sptNormal: Value := FDataLink.Field.AsInteger;
        sptFloat: FloatValue := FDataLink.Field.AsFloat;
        sptTime: TimeValue := FDataLink.Field.AsDateTime;
        sptDate: DateValue := FDataLink.Field.AsDateTime;
        sptHex: HexValue := FDataLink.Field.AsInteger;
        end;
  except
  end;
end;

procedure TDBAdvSpinEdit.DataUpdate(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
  begin
    if AllowNullValue and (self.Text = '') then
    begin
      FDataLink.Field.AsString := '';
    end
    else
//    if FDataLink.Field.IsNull then
//    begin
//      outputdebugstring('*');
//      self.Text := ''
//    end
//    else
      case SpinType of
      sptNormal: FDataLink.Field.AsInteger := self.Value;
      sptFloat: FDataLink.Field.AsFloat := self.FloatValue;
      sptTime: FDataLink.Field.AsDateTime := self.TimeValue;
      sptDate: FDataLink.Field.AsDateTime := self.DateValue;
      sptHex: FDataLink.Field.AsInteger := self.HexValue;
      end;
  end;
end;

destructor TDBAdvSpinEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

procedure TDBAdvSpinEdit.DownClick(Sender: TObject);
begin
  if FDataLink.Edit then
  begin
    FDataLink.Modified;
    inherited;
  end;
end;

function TDBAdvSpinEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBAdvSpinEdit.GetDataSource: TDataSource;
begin
 Result := FDataLink.DataSource;
end;

function TDBAdvSpinEdit.GetReadOnly: Boolean;
begin
 Result := FDataLink.ReadOnly;
end;

procedure TDBAdvSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FDataLink.ReadOnly and (key = VK_DELETE) then
    key := 0;

  inherited KeyDown(Key, Shift);

  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
  begin
    FDataLink.Edit;
    FDataLink.Modified;
  end;
end;

procedure TDBAdvSpinEdit.KeyPress(var Key: Char);
begin
  if not FDataLink.Edit then
  begin
    Exit;
  end;

  inherited KeyPress(Key);

  {$IFNDEF DELPHI_UNICODE}
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  if (Key >= #32) and (FDataLink.Field <> nil) and
  {$ENDIF}
    not FDataLink.Field.IsValidChar(Key) or (FDataLink.ReadOnly) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
     begin
      FDataLink.Edit;
      FDataLink.Modified;
     end;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

procedure TDBAdvSpinEdit.Loaded;
begin
 inherited;
end;

procedure TDBAdvSpinEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBAdvSpinEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBAdvSpinEdit.SetDataSource(const Value: TDataSource);
begin
 FDataLink.DataSource := Value;
end;

procedure TDBAdvSpinEdit.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TDBAdvSpinEdit.UpClick(Sender: TObject);
begin
  if FDataLink.Edit then
  begin
    FDataLink.Modified;
    inherited;
  end;
end;

procedure TDBAdvSpinEdit.WMChar(var Message: TWMKeyDown);
begin
  if FDataLink.Edit then
    inherited;
end;

procedure TDBAdvSpinEdit.WMCut(var Message: TMessage);
begin
  if FDataLink.Edit then
    inherited;
end;

procedure TDBAdvSpinEdit.WMPaste(var Message: TMessage);
begin
  if not FDataLink.Readonly then
  begin
    if FDataLink.Edit then
    begin
      FDataLink.Modified;
      inherited;
    end;
  end;
end;

procedure TDBAdvSpinEdit.WMUndo(var Message: TMessage);
begin
  if FDataLink.Edit then
    inherited;
end;

end.
