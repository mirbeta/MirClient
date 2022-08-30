{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2015                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvMultiInputQueryDialog;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, Classes, Controls, StdCtrls, ExtCtrls, AdvEdit, Graphics;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

resourcestring
  sOKButton = 'OK';
  sCancelButton = 'Cancel';

type
  TAdvCustomMultiInputQueryDialog = class;
  TAdvMultiInputQueryValue = class;

  TAdvEditQuery = class(TAdvEdit)
  private
    FQuery: TAdvMultiInputQueryValue;
  end;

  TAdvMultiInputQueryValue = class(TCollectionItem)
  private
    FEdit: TAdvEditQuery;
    FLbl: TLabel;
    FTag: Integer;
    FDatastring: string;
    FDataObject: TObject;
    FDataInteger: Integer;
    FMultiInputQuery: TAdvCustomMultiInputQueryDialog;
    FDataBoolean: Boolean;
    FDefaultValue: string;
    FLabel: string;
    FEditType: TAdvEditType;
    FResultValue: string;
    FName: string;
    FPasswordChar: Char;
    FHint: string;
    FSigned: Boolean;
    FCanUndo: Boolean;
    FPrecision: Integer;
    FFlatParentColor: Boolean;
    FLengthLimit: Integer;
    FModifiedColor: TColor;
    FPrompt: string;
    FExcelStyleDecimalSeparator: Boolean;
    FShowURL: Boolean;
    FFlat: Boolean;
    FShowModified: boolean;
    FEditAlign: TEditAlign;
    FValidChars: string;
    FFlatLineColor: TColor;
    FShowHint: boolean;
    FPrefix: string;
    FSuffix: string;
  public
    function MultiInputQuery: TAdvCustomMultiInputQueryDialog;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property Datastring: string read FDatastring write FDatastring;
    property DataInteger: Integer read FDataInteger write FDataInteger;
    property ResultValue: string read FResultValue;
    property LabelControl: TLabel read FLbl;
    property EditControl: TAdvEditQuery read FEdit;
  published
    property Tag: Integer read FTag write FTag default 0;
    property EditType: TAdvEditType read FEditType write FEditType default etString;
    property CanUndo: Boolean read FCanUndo write FCanUndo default True;
    property EditAlign: TEditAlign read FEditAlign write FEditAlign default eaLeft;
    property ExcelStyleDecimalSeparator: Boolean read FExcelStyleDecimalSeparator write FExcelStyleDecimalSeparator default False;
    property Flat: Boolean read FFlat write FFlat default false;
    property FlatLineColor: TColor read FFlatLineColor write FFlatLineColor default clBlack;
    property FlatParentColor: Boolean read FFlatParentColor write FFlatParentColor default True;
    property Hint: string read FHint write FHint;
    property LengthLimit: Integer read FLengthLimit write FLengthLimit default 0;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor default clHighlight;
    property PasswordChar: Char read FPasswordChar write FPasswordChar default #0;
    property Precision: Integer read FPrecision write FPrecision default 0;
    property Prefix: string read FPrefix write FPrefix;
    property Prompt: string read FPrompt write FPrompt;
    property ShowHint: boolean read FShowHint write FShowHint default False;
    property ShowModified: boolean read FShowModified write FShowModified default False;
    property ShowURL: Boolean read FShowURL write FShowURL default False;
    property Signed: Boolean read FSigned write FSigned default False;
    property Suffix: string read FSuffix write FSuffix;
    property ValidChars: string read FValidChars write FValidChars;

    property &Label: string read FLabel write FLabel;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property Name: string read FName write FName;
  end;

  TAdvMultiInputQueryValues = class(TOwnedCollection)
  private
    FMultiInputQuery: TAdvCustomMultiInputQueryDialog;
    function GetItem(Index: Integer): TAdvMultiInputQueryValue;
    procedure SetItem(Index: Integer; const Value: TAdvMultiInputQueryValue);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
  public
    function MultiInputQuery: TAdvCustomMultiInputQueryDialog;
    constructor Create(AMultiInputQuery: TAdvCustomMultiInputQueryDialog);
    function Add: TAdvMultiInputQueryValue;
    function Insert(Index: Integer): TAdvMultiInputQueryValue;
    property Items[Index: Integer]: TAdvMultiInputQueryValue read GetItem write SetItem; default;
  end;

  TAdvMultiInputQueryDialogCloseQueryEvent = procedure(Sender: TObject; var ACanClose: Boolean) of object;
  TAdvMultiInputQueryDialogCloseEvent = procedure(Sender: TObject) of object;
  TAdvMultiInputQueryDialogShowEvent = procedure(Sender: TObject) of object;
  TAdvMultiInputQueryDialogCustomizeFormEvent = procedure(Sender: TObject; AForm: TForm) of object;
  TAdvMultiInputQueryDialogValueValidateEvent = procedure(Sender: TObject; AValueIndex: Integer; AValue: String; var AIsValid: Boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvCustomMultiInputQueryDialog = class(TComponent)
  private
    FFormHeight, FFormWidth: Integer;
    FForm: TForm;
    FModal: Boolean;
    FPanel: TPanel;
    FOkButton: TButton;
    FCancelButton: TButton;
    FQueryValues: TAdvMultiInputQueryValues;
    FColumns: Integer;
    FOnClose: TAdvMultiInputQueryDialogCloseEvent;
    FOnCloseQuery: TAdvMultiInputQueryDialogCloseQueryEvent;
    FBorderStyle: TFormBorderStyle;
    FPosition: TPosition;
    FBorderIcons: TBorderIcons;
    FCaption: String;
    FOnCustomizeForm: TAdvMultiInputQueryDialogCustomizeFormEvent;
    FOnShow: TAdvMultiInputQueryDialogShowEvent;
    FEditWidth: Integer;
    FLabelWidth: Integer;
    FSpacing: Integer;
    FStayOnTop: Boolean;
    FOnValueValidate: TAdvMultiInputQueryDialogValueValidateEvent;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetQueryValues(const Value: TAdvMultiInputQueryValues);
    procedure SetColumns(const Value: Integer);
    function GetResultValue(AName: string): string;
    function GetValue(AName: string): TAdvMultiInputQueryValue;
  protected
    function GetVersionNr: Integer; virtual;
    procedure DoShow; virtual;
    procedure DoClose; virtual;
    procedure DoCloseQuery(var ACanClose: Boolean); virtual;
    procedure DoValueValidate(AValueIndex: Integer; AValue: String; var AIsValid: Boolean); virtual;
    procedure DoCustomizeForm(AForm: TForm); virtual;
    procedure CreateDefaultControls(AForm: TForm); virtual;
    procedure CreateQueryValues(AForm: TForm); virtual;
    procedure ButtonCancelClicked(Sender: TObject);
    procedure ButtonOkClicked(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var ACanClose: Boolean);
    procedure EditChanged(Sender: TObject);
    procedure EditValueValidate(Sender: TObject; Value: string; var IsValid: Boolean);
    property Version: string read GetVersion write SetVersion;
    property QueryValues: TAdvMultiInputQueryValues read FQueryValues write SetQueryValues;
    property Columns: Integer read FColumns write SetColumns default 1;
    property OnShow: TAdvMultiInputQueryDialogShowEvent read FOnShow write FOnShow;
    property OnClose: TAdvMultiInputQueryDialogCloseEvent read FOnClose write FOnClose;
    property OnCloseQuery: TAdvMultiInputQueryDialogCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnCustomizeForm: TAdvMultiInputQueryDialogCustomizeFormEvent read FOnCustomizeForm write FOnCustomizeForm;
    property OnValueValidate: TAdvMultiInputQueryDialogValueValidateEvent read FOnValueValidate write FOnValueValidate;
    property Position: TPosition read FPosition write FPosition default poScreenCenter;
    property StayOnTop: Boolean read FStayOnTop write FStayOnTop default False;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle default bsSingle;
    property BorderIcons: TBorderIcons read FBorderIcons write FBorderIcons default [biSystemMenu];
    property Caption: String read FCaption write FCaption;
    property LabelWidth: Integer read FLabelWidth write FLabelWidth default 50;
    property Spacing: Integer read FSpacing write FSpacing default 10;
    property EditWidth: Integer read FEditWidth write FEditWidth default 150;
  public
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy; override;
    function AddQueryValue(ALabel: String; ADefaultValue: String = ''): TAdvMultiInputQueryValue;
    function Execute(AModal: Boolean = True): TModalResult; overload;
    function Execute(AX, AY: Integer; AModal: Boolean = True): TModalResult; overload;
    procedure Assign(Source: TPersistent); override;
    property ResultValues[AName: string]: string read GetResultValue;
    property Values[AName: string]: TAdvMultiInputQueryValue read GetValue;
  end;

  TAdvMultiInputQueryDialog = class(TAdvCustomMultiInputQueryDialog)
  published
    property Caption;
    property BorderIcons;
    property Version;
    property QueryValues;
    property Columns;
    property OnClose;
    property OnShow;
    property OnCloseQuery;
    property OnCustomizeForm;
    property OnValueValidate;
    property Position;
    property BorderStyle;
    property StayOnTop;
    property LabelWidth;
    property Spacing;
    property EditWidth;
  end;

function ShowMultiInputQueryPos(const ACaption: string; const ALabels: array of string; const ADefaultValues: array of string; var AValues: array of string; AX, AY: Integer): Boolean;
function ShowMultiInputQuery(const ACaption: string; const ALabels: array of string; const ADefaultValues: array of string; var AValues: array of string): Boolean;
function ShowInputQuery(const ACaption: string; ALabel: String; ADefaultValue: String; var AValue: String): Boolean;

implementation

uses
  SysUtils, Math;

function ShowMultiInputQueryPos(const ACaption: string; const ALabels: array of string; const ADefaultValues: array of string; var AValues: array of string; AX, AY: Integer): Boolean; overload;
var
  d: TAdvMultiInputQueryDialog;
  I: Integer;
  qv: TAdvMultiInputQueryValue;
begin
  if Length(AValues) < Length(ALabels) then
    raise EInvalidOperation.Create('Length of value array must be >= length of label array');

  d := TAdvMultiInputQueryDialog.Create(nil);
  try
    d.Caption := ACaption;
    if (AX > -1) and (AY > -1) then
      d.Position := poDesigned;

    for I := 0 to Length(ALabels) - 1 do
    begin
      qv := d.QueryValues.Add;
      qv.&Label := ALabels[I];
      if (I >= 0) and (I <= Length(ADefaultValues) - 1) then
        qv.DefaultValue := ADefaultValues[I];
    end;

    Result := d.Execute(AX, AY) = mrOk;
    for I := 0 to d.QueryValues.Count - 1 do
    begin
      qv := d.QueryValues[I];
      AValues[I] := qv.ResultValue;
    end;
  finally
    d.Free;
  end;
end;

function ShowMultiInputQuery(const ACaption: string; const ALabels: array of string; const ADefaultValues: array of string; var AValues: array of string): Boolean; overload;
begin
  Result := ShowMultiInputQueryPos(ACaption, ALabels, ADefaultValues, AValues, -1, -1);
end;

function ShowInputQuery(const ACaption: String; ALabel: String; ADefaultValue: String; var AValue: String): Boolean;
var
  v: array of string;
begin
  SetLength(v, 1);
  Result := ShowMultiInputQuery(ACaption, [ALabel], [ADefaultValue], v);
  AValue := v[0];
end;

{ TAdvCustomMultiInputQueryDialog }

procedure TAdvCustomMultiInputQueryDialog.CreateDefaultControls(AForm: TForm);
begin
  FPanel := TPanel.Create(Self);
  FPanel.Align := TAlign.alBottom;

  FCancelButton := TButton.Create(FPanel);
  FCancelButton.OnClick := ButtonCancelClicked;
  FCancelButton.Caption := sCancelButton;
  FCancelButton.Align := TAlign.alRight;
  FCancelButton.AlignWithMargins := True;
  FCancelButton.Margins.Left := 5;
  FCancelButton.Margins.Top := 5;
  FCancelButton.Margins.Right := 5;
  FCancelButton.Margins.Bottom := 5;
  FCancelButton.ModalResult := mrCancel;

  FOkButton := TButton.Create(FPanel);
  FOkButton.OnClick := ButtonOkClicked;
  FOkButton.Caption := sOKButton;
  FOkButton.Align := TAlign.alRight;
  FOkButton.AlignWithMargins := True;
  FOkButton.Margins.Left := 5;
  FOkButton.Margins.Top := 5;
  FOkButton.Margins.Right := 5;
  FOkButton.Margins.Bottom := 5;
  FOkButton.Default := True;
  FOkButton.ModalResult := mrOk;

  FPanel.Height := FCancelButton.Height + 10;
  FOkButton.Parent := FPanel;
  FCancelButton.Parent := FPanel;
  FPanel.Parent := AForm;
  FPanel.Ctl3D := False;
  FPanel.BevelOuter := bvNone;

  FFormHeight := FFormHeight + FPanel.Height;
end;

procedure TAdvCustomMultiInputQueryDialog.CreateQueryValues(AForm: TForm);
var
  I: Integer;
  ed: TAdvEditQuery;
  lbl: TLabel;
  x, y, num, K: integer;
  qv: TAdvMultiInputQueryValue;
  maxh, spc, maxw: Integer;
begin
  if Columns <= 0 then
    Exit;

  y := 10;
  x := 10;
  spc := Spacing;
  I := 0;
  maxh := 0;
  maxw := 0;
  K := 0;
  num := Floor((QueryValues.Count / Columns) + 0.5);
  while i < QueryValues.Count do
  begin
    qv := QueryValues[I];

    lbl := TLabel.Create(Self);
    qv.FLbl := lbl;
    lbl.Parent := AForm;
    lbl.Caption := qv.&Label;
    lbl.Width := LabelWidth;
    lbl.Left := x;
    lbl.Top := y;
    lbl.EllipsisPosition := epEndEllipsis;

    ed := TAdvEditQuery.Create(AForm);
    ed.OnValueValidate := EditValueValidate;
    qv.FEdit := ed;
    ed.FQuery := qv;
    ed.Parent := AForm;
    ed.EditType := qv.EditType;

    ed.MaxLength := 255;
    ed.CanUndo := qv.CanUndo;
    ed.EditType := qv.EditType;
    ed.EditAlign := qv.EditAlign;
    ed.ExcelStyleDecimalSeparator := qv.ExcelStyleDecimalSeparator;
    ed.Precision := qv.Precision;
    ed.Prefix := qv.Prefix;
    ed.Suffix := qv.Suffix;
    ed.Flat := qv.Flat;
    ed.FocusColor := clNone;
    ed.Color := clWhite;
    ed.FlatLineColor := qv.FlatLineColor;
    ed.FlatParentColor := qv.FlatParentColor;
    ed.LengthLimit := qv.LengthLimit;
    ed.ShowModified := qv.ShowModified;
    ed.ModifiedColor := qv.ModifiedColor;
    ed.ShowURL := qv.ShowURL;
    ed.PasswordChar := qv.PasswordChar;
    ed.Signed := qv.Signed;
    ed.Hint := qv.Hint;
    ed.ShowHint := qv.ShowHint;
    ed.ValidChars := qv.ValidChars;
    if ed.Flat then
      ed.Transparent := true;

    ed.DefaultHandling := false;

    ed.Width := EditWidth;
    ed.Left := lbl.Left + lbl.Width + spc;
    ed.Top := y;
    ed.Text := qv.DefaultValue;
    ed.OnChange := EditChanged;
    qv.FResultValue := qv.DefaultValue;

    ed.Top := y + (lbl.Height - ed.Height) div 2;

    y := y + 10 + ed.Height;
    if y - 10 > maxh then
      maxh := y - 10;

    if x + LabelWidth + spc + EditWidth + 10 > maxw then
      maxw := x + LabelWidth + spc + EditWidth + 10;

    Inc(I);
    Inc(K);
    if K = num then
    begin
      x := maxw;
      y := 10;
      K := 0;
    end;
  end;

  FFormHeight := FFormHeight + maxh;
  FFormWidth := FFormWidth + maxw;
end;

destructor TAdvCustomMultiInputQueryDialog.Destroy;
begin
  FQueryValues.Free;
  inherited;
end;

procedure TAdvCustomMultiInputQueryDialog.DoClose;
begin
  if Assigned(OnClose) then
    OnClose(Self);
end;

procedure TAdvCustomMultiInputQueryDialog.DoCloseQuery(var ACanClose: Boolean);
begin
  if Assigned(OnCloseQuery) then
    OnCloseQuery(Self, ACanClose);
end;

procedure TAdvCustomMultiInputQueryDialog.DoCustomizeForm(AForm: TForm);
begin
  if Assigned(OnCustomizeForm) then
    OnCustomizeForm(Self, AForm);
end;

procedure TAdvCustomMultiInputQueryDialog.DoShow;
begin
  if Assigned(OnShow) then
    OnShow(Self);
end;

procedure TAdvCustomMultiInputQueryDialog.DoValueValidate(AValueIndex: Integer;
  AValue: String; var AIsValid: Boolean);
begin
  if Assigned(OnValueValidate) then
    OnValueValidate(Self, AValueIndex, AValue, AIsValid);
end;

procedure TAdvCustomMultiInputQueryDialog.EditChanged(Sender: TObject);
var
  qv: TAdvMultiInputQueryValue;
  ed: TAdvEditQuery;
begin
  ed := Sender as TAdvEditQuery;
  if Assigned(ed) then
  begin
    qv := ed.FQuery;
    if Assigned(qv) then
      qv.FResultValue := ed.Text;
  end;
end;

procedure TAdvCustomMultiInputQueryDialog.EditValueValidate(Sender: TObject;
  Value: string; var IsValid: Boolean);
var
  ed: TAdvEditQuery;
begin
  ed := Sender as TAdvEditQuery;
  if Assigned(ed) and Assigned(ed.FQuery) then
    DoValueValidate(ed.FQuery.Index, Value, IsValid);
end;

function TAdvCustomMultiInputQueryDialog.Execute(AX, AY: Integer;
  AModal: Boolean): TModalResult;
begin
  Result := mrNone;
  if Assigned(FForm) then
    Exit;

  if QueryValues.Count = 0 then
    raise Exception.Create('No query values, please add a query value first.');

  FModal := AModal;
  FForm := TForm.Create(Application);
  try
    FForm.OnClose := FormClose;
    FForm.OnCloseQuery := FormCloseQuery;
    FForm.OnShow := FormShow;
    FForm.Caption := Caption;
    FForm.Position := poScreenCenter;
    if (AX > -1) and (AY > -1) then
    begin
      FForm.Left := AX;
      FForm.Top := AY;
    end;
    FForm.Position := Position;
    FForm.BorderIcons := BorderIcons;
    if StayOnTop then
      FForm.FormStyle := fsStayOnTop;

    FForm.BorderStyle := BorderStyle;
    FFormHeight := 0;
    FFormWidth := 0;
    CreateDefaultControls(FForm);
    CreateQueryValues(FForm);
    FForm.ClientHeight := Max(50, FFormHeight);
    FForm.ClientWidth := Max(200, FFormWidth);

    DoCustomizeForm(FForm);

    if AModal then
      Result := FForm.ShowModal
    else
    begin
      FForm.Show;
      Result := mrOk;
    end;
  finally
  end;
end;

function TAdvCustomMultiInputQueryDialog.Execute(AModal: Boolean = True): TModalResult;
begin
  Result := Execute(-1, -1, AModal);
end;

procedure TAdvCustomMultiInputQueryDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  FForm := nil;
  DoClose;
end;

procedure TAdvCustomMultiInputQueryDialog.FormCloseQuery(Sender: TObject; var ACanClose: Boolean);
begin
  DoCloseQuery(ACanClose);
end;

procedure TAdvCustomMultiInputQueryDialog.FormShow(Sender: TObject);
begin
  if QueryValues.Count > 0 then
  begin
    if Assigned(QueryValues[0].FEdit) then
      QueryValues[0].FEdit.SetFocus;
  end;

  DoShow;
end;

function TAdvCustomMultiInputQueryDialog.AddQueryValue(ALabel,
  ADefaultValue: String): TAdvMultiInputQueryValue;
begin
  Result := QueryValues.Add;
  Result.&Label := ALabel;
  Result.DefaultValue := ADefaultValue;
end;

procedure TAdvCustomMultiInputQueryDialog.Assign(Source: TPersistent);
begin
  if Source is TAdvCustomMultiInputQueryDialog then
    FQueryValues.Assign((Source as TAdvMultiInputQueryDialog).QueryValues);
end;

procedure TAdvCustomMultiInputQueryDialog.ButtonCancelClicked(Sender: TObject);
begin
  if Assigned(FForm) and not FModal then
  begin
    FForm.ModalResult := mrCancel;
    FForm.Close;
  end;
end;

procedure TAdvCustomMultiInputQueryDialog.ButtonOkClicked(Sender: TObject);
begin
  if Assigned(FForm) and not FModal then
  begin
    FForm.ModalResult := mrOk;
    FForm.Close;
  end;
end;

constructor TAdvCustomMultiInputQueryDialog.Create(AOwner: TComponent);
begin
  inherited;
  FQueryValues := TAdvMultiInputQueryValues.Create(Self);
  FColumns := 1;
  FBorderStyle := bsSingle;
  FStayOnTop := False;
  FPosition := poScreenCenter;
  FBorderIcons := [biSystemMenu];
  FCaption := 'Multi Input Query';
  FLabelWidth := 50;
  FEditWidth := 150;
  FSpacing := 10;
end;

function TAdvCustomMultiInputQueryDialog.GetResultValue(AName: string): string;
var
  qv: TAdvMultiInputQueryValue;
begin
  Result := '';
  qv := Values[AName];
  if Assigned(qv) then
    Result := qv.ResultValue;
end;

function TAdvCustomMultiInputQueryDialog.GetValue(
  AName: string): TAdvMultiInputQueryValue;
var
  I: Integer;
  qv: TAdvMultiInputQueryValue;
begin
  Result := nil;
  for I := 0 to QueryValues.Count - 1 do
  begin
    qv := QueryValues[I];
    if qv.Name = AName then
    begin
      Result := qv;
      Break;
    end;
  end;
end;

function TAdvCustomMultiInputQueryDialog.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomMultiInputQueryDialog.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvCustomMultiInputQueryDialog.SetColumns(const Value: Integer);
begin
  FColumns := Max(1, Value);
end;

procedure TAdvCustomMultiInputQueryDialog.SetQueryValues(
  const Value: TAdvMultiInputQueryValues);
begin
  FQueryValues.Assign(Value);
end;

procedure TAdvCustomMultiInputQueryDialog.SetVersion(const Value: string);
begin

end;

{ TAdvMultiInputQueryValue }

procedure TAdvMultiInputQueryValue.Assign(Source: TPersistent);
begin
  if Source is TAdvMultiInputQueryValue then
  begin
    FTag := (Source as TAdvMultiInputQueryValue).Tag;
    FEditType := (Source as TAdvMultiInputQueryValue).EditType;
    FDefaultValue := (Source as TAdvMultiInputQueryValue).DefaultValue;
    FCanUndo := (Source as TAdvMultiInputQueryValue).CanUndo;
    FEditAlign := (Source as TAdvMultiInputQueryValue).EditAlign;
    FFlat := (Source as TAdvMultiInputQueryValue).Flat;
    FExcelStyleDecimalSeparator := (Source as TAdvMultiInputQueryValue).ExcelStyleDecimalSeparator;
    FFlatLineColor := (Source as TAdvMultiInputQueryValue).FlatLineColor;
    FFlatParentColor := (Source as TAdvMultiInputQueryValue).FlatParentColor;
    FHint := (Source as TAdvMultiInputQueryValue).Hint;
    FLengthLimit := (Source as TAdvMultiInputQueryValue).LengthLimit;
    FModifiedColor := (Source as TAdvMultiInputQueryValue).ModifiedColor;
    FPasswordChar := (Source as TAdvMultiInputQueryValue).PasswordChar;
    FPrecision := (Source as TAdvMultiInputQueryValue).Precision;
    FPrefix := (Source as TAdvMultiInputQueryValue).Prefix;
    FPrompt := (Source as TAdvMultiInputQueryValue).Prompt;
    FShowHint := (Source as TAdvMultiInputQueryValue).ShowHint;
    FShowModified := (Source as TAdvMultiInputQueryValue).ShowModified;
    FShowURL := (Source as TAdvMultiInputQueryValue).ShowURL;
    FSigned := (Source as TAdvMultiInputQueryValue).Signed;
    FSuffix := (Source as TAdvMultiInputQueryValue).Suffix;
    FValidChars := (Source as TAdvMultiInputQueryValue).ValidChars;
  end;
end;

constructor TAdvMultiInputQueryValue.Create(Collection: TCollection);
begin
  inherited;
  FMultiInputQuery := (Collection as TAdvMultiInputQueryValues).MultiInputQuery;
  FEditType := etString;
  FCanUndo := True;
  FEditAlign := eaLeft;
  FFlat := False;
  FExcelStyleDecimalSeparator := False;
  FFlatLineColor := clBlack;
  FFlatParentColor := True;
  FHint := '';
  FLengthLimit := 0;
  FModifiedColor := clHighlight;
  FPasswordChar := #0;
  FPrecision := 0;
  FPrefix := '';
  FPrompt := '';
  FShowHint := False;
  FShowModified := False;
  FShowURL := False;
  FSigned := False;
  FSuffix := '';
  FValidChars := '';
end;

destructor TAdvMultiInputQueryValue.Destroy;
begin
  inherited;
end;

function TAdvMultiInputQueryValue.MultiInputQuery: TAdvCustomMultiInputQueryDialog;
begin
  Result := FMultiInputQuery;
end;

{ TAdvMultiInputQueryValues }

function TAdvMultiInputQueryValues.Add: TAdvMultiInputQueryValue;
begin
  Result := TAdvMultiInputQueryValue(inherited Add);
end;

constructor TAdvMultiInputQueryValues.Create(AMultiInputQuery: TAdvCustomMultiInputQueryDialog);
begin
  inherited Create(AMultiInputQuery, GetItemClass);
  FMultiInputQuery := AMultiInputQuery;
end;

function TAdvMultiInputQueryValues.GetItem(Index: Integer): TAdvMultiInputQueryValue;
begin
  Result := TAdvMultiInputQueryValue(inherited Items[Index]);
end;

function TAdvMultiInputQueryValues.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvMultiInputQueryValue;
end;

function TAdvMultiInputQueryValues.Insert(Index: Integer): TAdvMultiInputQueryValue;
begin
  Result := TAdvMultiInputQueryValue(inherited Insert(Index));
end;

function TAdvMultiInputQueryValues.MultiInputQuery: TAdvCustomMultiInputQueryDialog;
begin
  Result := FMultiInputQuery;
end;

procedure TAdvMultiInputQueryValues.SetItem(Index: Integer;
  const Value: TAdvMultiInputQueryValue);
begin
  inherited Items[Index] := Value;
end;

{$IFDEF FREEWARE}
function Scramble(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  for i := 1 to length(s) do
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;
{$ENDIF}
initialization
begin
{$IFDEF FREEWARE}
   if  (FindWindow(PChar(Scramble('QDuuilfdqljk')), nil) = 0) OR
       (FindWindow(PChar(Scramble('QDuuGplia`w')), nil) = 0) then
   begin
     MessageBox(0,PChar(Scramble('Duuilfdqljk%pv`v%qwldi%s`wvljk%jc%QHV%vjcqrdw`%fjhujk`kqv+')+#13#10+Scramble('Fjkqdfq%QHV%vjcqrdw`%mqqu?**rrr+qhvvjcqrdw`+fjh%cjw%sdila%ilf`kvlkb+')),PChar(Scramble('Rdwklkb')),MB_OK);
   end;
{$ENDIF}
end;

end.

