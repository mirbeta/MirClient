{********************************************************************}
{ TDBMONEYEDIT component                                             }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ TMS Software                                                       }
{ copyright © 1998-2012                                              }
{ Email : info@tmssoftware.com                                       }
{ Web : http://www.tmssoftware.com                                   }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit dbmnyed;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, MoneyEdit, db, dbctrls;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBMoneyEdit = class(TMoneyEdit)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure DataUpdate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure CMExit(var Message: TWMNoParams); message CM_EXIT;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure ResetMaxLength;
    function GetTextMargins: TPoint;
  protected
    { Protected declarations }
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure CalcChange; override;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;



implementation


type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;



{ TDBMoneyEdit }


procedure TDBMoneyEdit.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString , ftWideString  ]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

procedure TDBMoneyEdit.Change;
begin
  if Assigned(FDataLink) then
    begin
     FDataLink.Modified;
    end;
  inherited;
end;

procedure TDBMoneyEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

procedure TDBMoneyEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBMoneyEdit.CMExit(var Message: TWMNoParams);
begin
 if not FDataLink.ReadOnly then
  begin
   try
      FDataLink.UpdateRecord;                          { tell data link to update database }
   except
      on Exception do SetFocus;                      { if it failed, don't let focus leave }
   end;
  end;
  inherited;
end;

procedure TDBMoneyEdit.CMEnter(var Message: TWMNoParams);
begin
 inherited;
 if FDataLink.CanModify then inherited ReadOnly := False;
end;

constructor TDBMoneyEdit.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := DataUpdate;
  ControlStyle := ControlStyle + [csReplicatable];
end;

procedure TDBMoneyEdit.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and not (fDataLink.BOF and fDataLink.EOF) then
  begin
    self.Text := FDataLink.Field.AsString;
    Modified := false;
  end;

  if not Assigned(FDataLink.Field) or (FDataLink.BOF and FDataLink.EOF) then
  begin
    self.Text := '';
  end;

end;

procedure TDBMoneyEdit.DataUpdate(Sender: TObject);
begin
 if assigned(FDataLink.Field) and not (fDataLink.BOF and fDataLink.EOF)  then
  begin
    FDataLink.Field.AsString := self.Text;
  end;
end;

destructor TDBMoneyEdit.Destroy;
begin
  FDataLink.Free;      
  inherited Destroy;
end;


function TDBMoneyEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBMoneyEdit.GetDataSource: TDataSource;
begin
 Result := FDataLink.DataSource;
end;

function TDBMoneyEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBMoneyEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then ResetMaxLength;
  FDataLink.FieldName := Value;
end;

procedure TDBMoneyEdit.SetDataSource(const Value: TDataSource);
begin
 FDataLink.DataSource := Value;
end;

procedure TDBMoneyEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TDBMoneyEdit.WMCut(var Message: TWMCut);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDBMoneyEdit.WMPaste(var Message: TWMPaste);
begin
  if not FDataLink.Readonly then
   begin
    FDataLink.Edit;
    inherited;
   end;
end;

procedure TDBMoneyEdit.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDBMoneyEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FDataLink.ReadOnly and (key=VK_DELETE) then key:=0;
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDBMoneyEdit.KeyPress(var Key: Char);
begin
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
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll; 
        Key := #0;
      end;
  end;

end;

procedure TDBMoneyEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
end;


procedure TDBMoneyEdit.WMPaint(var Message: TWMPaint);
var
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  AAlignment: TAlignment;

begin

 if  not (csPaintCopy in ControlState) then inherited
 else
  begin
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;

  AAlignment := taLeftJustify;

  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := Color;
      if not Enabled then
        Font.Color := clGrayText;
      if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end else
        S := Text;
      if PasswordChar <> #0 then FillChar(S[1], Length(S), PasswordChar);

      Margins := GetTextMargins;
      case AAlignment of
        taLeftJustify: Left := Margins.X;
        taRightJustify: Left := ClientWidth - TextWidth(S) - Margins.X - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      if SysLocale.MiddleEast then UpdateTextFlags;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;

  end;
end;

function TDBMoneyEdit.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then I := 0 else
      if Ctl3D then I := 1 else I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then I := 0 else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;



function TDBMoneyEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDBMoneyEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;




procedure TDBMoneyEdit.CalcChange;
begin
  inherited;
  if assigned(fDataLink) then fDataLink.Edit;
end;

end.
