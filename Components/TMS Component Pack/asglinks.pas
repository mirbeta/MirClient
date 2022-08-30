{**************************************************************************}
{ TADVSTRINGGRID EDITLINKS                                                 }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2000-2015                                         }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AsgLinks;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, Graphics, Forms, SysUtils,
  MoneyEdit, AdvEdit, CListEd, AdvGrid, ColorCombo, ImagePicker, ShellApi,
  AdvFileNameEdit, AdvDirectoryEdit, Dialogs, LUCombo;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvEditEditLink = class(TEditLink)
  private
    FEdit: TAdvEdit;
    FEditColor: TColor;
    FModifiedColor: TColor;
    FEditType: TAdvEditType;
    FSuffix: String;
    FPrefix: String;
    FEditAlign: TEditAlign;
    FShowModified: Boolean;
    FPrecision: Integer;
    FSigned: Boolean;
    FExcelStyleDecimalSeparator: Boolean;
    FMaxLength: Integer;
    FValidChars: string;
  protected
    procedure EditExit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetFocus(Value: Boolean); override;
    procedure SetProperties; override;
    procedure SetRect(r: TRect); override;
  published
    property EditAlign: TEditAlign read FEditAlign Write FEditAlign;
    property EditColor: TColor read FEditColor Write FEditColor;
    property MaxLength: Integer read FMaxLength write FMaxLength;
    property ModifiedColor: TColor read FModifiedColor Write FModifiedColor;
    property EditType: TAdvEditType read FEditType Write FEditType;
    property Prefix: String read FPrefix Write FPrefix;
    property ShowModified: boolean read FShowModified Write FShowModified;
    property Suffix: String read FSuffix Write FSuffix;
    property Precision: integer read FPrecision Write FPrecision;
    property Signed: boolean read FSigned write FSigned;
    property ExcelStyleDecimalSeparator: boolean read FExcelStyleDecimalSeparator write FExcelStyleDecimalSeparator;
    property ValidChars: string read FValidChars write FValidChars;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvFileNameEditLink = class(TEditLink)
  private
    FEdit: TAdvFileNameEdit;
    FModifiedColor: TColor;
    FEditColor: TColor;
    FShowModified: boolean;
    FFilterIndex: Integer;
    FFilter: string;
    FInitialDir: string;
    FDialogTitle: string;
    FDialogKind: TFileDialogKind;
    FDialogOptions: TOpenOptions;
    FDefaultExt : String;
  protected
    Procedure EditExit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
  published
    property EditColor: TColor read FEditColor write FEditColor;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor;
    property ShowModified: boolean read FShowModified write FShowModified;
    property Filter: string read FFilter write FFilter;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex default 1;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DialogOptions: TOpenOptions read FDialogOptions write FDialogOptions default [ofHidereadOnly, ofEnableSizing];
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DialogKind: TFileDialogKind read FDialogKind write FDialogKind;
    Property DefaultExt: string read FDefaultExt write FDefaultExt;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDirectoryEditLink = class(TEditLink)
  private
    FEdit: TAdvDirectoryEdit;
    FShowModified: Boolean;
    FModifiedColor: TColor;
    FEditColor: TColor;
  protected
    Procedure EditExit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
  published
    property EditColor: TColor read FEditColor write FEditColor;
    property ShowModified: Boolean read FShowModified write FShowModified;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TColorComboEditLink = class(TEditLink)
  private
    FEdit: TAdvColorComboBox;
    FDropDownHeight: integer;
    FDropDownWidth: integer;
  protected
    Procedure EditExit(Sender: TObject);
  public
    Constructor Create(aOwner: TComponent); override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    procedure SetRect(r: trect); override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
  published
    property DropDownHeight: integer read FDropDownHeight write FDropDownHeight;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TMoneyEditLink = class(TEditLink)
  Private
    FEdit: TMoneyEdit;
    FCalculatorLook: TCalculatorLook;
    Procedure SetCalculatorLook(Const Value: TCalculatorLook);
  protected
    Procedure EditExit(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
    procedure SetCellProps(AColor: TColor; AFont: TFont); override;
  Published
    Property CalculatorLook: TCalculatorLook read FCalculatorLook write SetCalculatorLook;
  End;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCheckEditLink = class(TEditLink)
  Private
    FEdit: TCheckListEdit;
  protected
    procedure EditExit(sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetCellProps(AColor: TColor; AFont: TFont); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TImagePickerEditLink = class(TEditLink)
  private
    FEdit: TImagePicker;
    FDropDownHeight: integer;
    FDropDownWidth: integer;
    FImages: TImageList;
  protected
    procedure EditExit(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure CreateEditor(AParent: TWinControl); override;
    procedure DestroyEditor; override;
    procedure SetRect(r: trect); override;
    function GetEditorValue: String; override;
    procedure SetEditorValue(s: String); override;
    function GetEditControl: TWinControl; override;
    procedure SetProperties; override;
  published
    property DropDownHeight: integer read FDropDownHeight write FDropDownHeight;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth;
    property Images: TImageList read FImages write FImages;
  end;

// class definition
// Lookup column combo box editor class

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TLUComboEditLink = class(TEditLink)
  private
    FCellHeight: Integer;
    FItems: TStrings;
    FFlat: Boolean;
    FDropHeight: Integer;
    FDropWidth: Integer;
    FEtched: Boolean;
    procedure SetItems(const Value: TStrings);
  protected
    procedure EditExit(Sender: TObject);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    Combo: TLUCombo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateEditor(AParent: TWinControl); override;
    function GetEditorValue:string; override;
    procedure SetEditorValue(s:string); override;
    function GetEditControl:TWinControl; override;
    procedure SetProperties; override;
    {$IFDEF ASG194}
    procedure SetCellProps(AColor: TColor; AFont: TFont); override;
    {$ENDIF}
    procedure SetRect(R: TRect); override;
  published
    property DropHeight: Integer read FDropHeight write FDropHeight;
    property DropWidth: Integer read FDropWidth write FDropWidth;
    property Etched: Boolean read FEtched write FEtched;
    property Flat: Boolean read FFlat write FFlat;
    property Items: TStrings read FItems write SetItems;
  end;


procedure Register;

implementation

{$I DELPHIXE.INC}

type
  TMyWinControl = class(TWinControl)
  published
 //   property OnExit;
  end;

function StripThousandSep(s: string): string;
begin
  while (Pos(ThousandSeparator, s) > 0) do
    Delete(s, Pos(ThousandSeparator, s), 1);
  Result := s;
end;


procedure Register;
begin
  RegisterComponents('TMS Grids', [TAdvEditEditLink, TColorComboEditLink,
    TCheckEditLink, TMoneyEditLink, TImagePickerEditLink,
    TAdvFileNameEditLink,
    TAdvDirectoryEditLink,
    TLUComboEditLink]);
end;

{ TAdvEditEditLink }

Procedure TAdvEditEditLink.CreateEditor(AParent: TWinControl);
Begin
  FEdit := TAdvEdit.Create(Grid);
  FEdit.ShowModified := True;
  FEdit.DefaultHandling := False;
  FEdit.ModifiedColor := clRed;
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  WantKeyLeftRight := True;
  WantKeyHomeEnd := True;
  FEdit.Parent := AParent;
  FEdit.Color := EditColor;
  FEdit.DefaultHandling := True;
  FEdit.Flat := true;
  FEdit.FlatLineColor := clNone;
End;

Procedure TAdvEditEditLink.DestroyEditor;
Begin
  If Assigned(FEdit) Then
    FEdit.Free;
  FEdit := Nil;
End;

Function TAdvEditEditLink.GetEditorValue: String;
Begin
  if Assigned(FEdit) then
  begin
//    if EditType = etMoney then
//      Result := StripThousandSep(FEdit.Text)
//    else
      Result := FEdit.Text;
  end;
End;

Procedure TAdvEditEditLink.EditExit(sender: TObject);
Begin
  HideEditor;
End;

Function TAdvEditEditLink.GetEditControl: TWinControl;
Begin
  Result := FEdit;
End;

Procedure TAdvEditEditLink.SetEditorValue(s: String);
Begin
  FEdit.Text := s;
End;

procedure TAdvEditEditLink.SetFocus(Value: Boolean);
begin
  inherited;
  if FEdit.AutoSelect = false then
    FEdit.SelStart := Length(FEdit.Text);
end;

Constructor TAdvEditEditLink.Create(aOwner: TComponent);
Begin
  Inherited;
  WantKeyLeftRight := True;
  WantKeyHomeEnd := True;
  EditColor := clWindow;
  ModifiedColor := clRed;
  EditType := etString;
End;

Procedure TAdvEditEditLink.SetProperties;
Begin
  Inherited;
  FEdit.Color := FEditColor;
  FEdit.FocusColor := FEditColor;
  FEdit.EditType := FEditType;
  FEdit.EditAlign := FEditAlign;
  FEdit.ModifiedColor := FModifiedColor;
  FEdit.Prefix := FPrefix;
  FEdit.Suffix := FSuffix;
  FEdit.ShowModified := FShowModified;
  FEdit.MaxLength := FMaxLength;
  FEdit.Precision := FPrecision;
  FEdit.Signed := FSigned;
  FEdit.ValidChars := FValidChars;
  FEdit.ExcelStyleDecimalSeparator := FExcelStyleDecimalSeparator;
End;

procedure TAdvEditEditLink.SetRect(r: TRect);
begin
  inherited;
end;

Destructor TAdvEditEditLink.Destroy;
Begin
  Inherited;
End;

{ TColorComboEditLink }

Procedure TColorComboEditLink.CreateEditor(AParent: TWinControl);
Begin
  FEdit := TAdvColorComboBox.Create(Grid);
  FEdit.Style := csOwnerDrawFixed;
  FEdit.OnExit := EditExit;
  FEdit.OnKeydown := EditKeyDown;
  FEdit.Width := 0;
  FEdit.Height := 0;
  FEdit.IsWinXP := Grid.IsThemed;
  FEdit.Parent := AParent;
End;

Procedure TColorComboEditLink.DestroyEditor;
Begin
  FEdit.Free;
End;

Function TColorComboEditLink.GetEditorValue: String;
Begin
  Result := FEdit.Items[FEdit.ItemIndex];
End;

Procedure TColorComboEditLink.EditExit(sender: TObject);
Begin
  HideEditor;
End;

Function TColorComboEditLink.GetEditControl: TWinControl;
Begin
  Result := FEdit;
End;

Procedure TColorComboEditLink.SetRect(r: TRect);
Begin
  Inherited;
  FEdit.Height := r.Bottom - r.Top + FDropDownHeight;
End;

Procedure TColorComboEditLink.SetEditorValue(s: String);
Var
  i: Integer;
Begin
  FEdit.Items.Clear;
  For i := 0 To 15 Do
    FEdit.Items.Add(IntToStr(i));
  If s = '' Then
    s := '0';
  FEdit.Text := s;
  FEdit.ItemIndex := StrToInt(s);
End;

Constructor TColorComboEditLink.Create(aOwner: TComponent);
Begin
  Inherited;
  WantKeyUpDown := True;
  DropDownWidth := 100;
  DropDownHeight := 100;
End;

Procedure TColorComboEditLink.SetProperties;
Begin
  Inherited;
  FEdit.DropWidth := 150;
End;



{ TMoneyEditLink }

Constructor TMoneyEditLink.Create(aOwner: TComponent);
Begin
  Inherited;
  WantKeyLeftRight := True;
  WantKeyHomeEnd := True;
  FCalculatorLook := TCalculatorLook.Create;
End;

Procedure TMoneyEditLink.CreateEditor(AParent: TWinControl);
Begin
  Inherited;
  FEdit := TMoneyEdit.Create(Grid);
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeydown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  FEdit.Parent := AParent;

  FEdit.CalculatorLook.Flat := True;
  FEdit.CalculatorLook.ButtonColor := clBlue;
  FEdit.CalculatorLook.Color := clYellow;
  FEdit.CalculatorLook.Font.Color := clWhite;
  FEdit.CalculatorLook.Font.Name := 'Tahoma';
  FEdit.CalculatorLook.Font.Style := [fsBold];
End;

Destructor TMoneyEditLink.Destroy;
Begin
  FCalculatorLook.Free;
  Inherited;
End;

Procedure TMoneyEditLink.DestroyEditor;
Begin
  FEdit.Free;
End;

Procedure TMoneyEditLink.EditExit(sender: TObject);
Begin
  HideEditor;
End;

Function TMoneyEditLink.GetEditControl: TWinControl;
Begin
  Result := FEdit;
End;

Function TMoneyEditLink.GetEditorValue: String;
Begin
  Result := FEdit.Text;
End;

Procedure TMoneyEditLink.SetCalculatorLook(Const Value: TCalculatorLook);
Begin
  FCalculatorLook.Assign(Value);
End;

procedure TMoneyEditLink.SetCellProps(AColor: TColor; AFont: TFont);
begin
  inherited;
  FEdit.Color := AColor;
end;

Procedure TMoneyEditLink.SetEditorValue(s: String);
Begin
  FEdit.Text := s;
End;

Procedure TMoneyEditLink.SetProperties;
Begin
  Inherited;
  FEdit.CalculatorLook.Assign(FCalculatorLook);
End;

{ TCheckEditLink }

Constructor TCheckEditLink.Create(aOwner: TComponent);
Begin
  Inherited;
  WantKeyUpDown := True;
End;

Procedure TCheckEditLink.CreateEditor(AParent: TWinControl);
Begin
  Inherited;
  FEdit := TCheckListEdit.Create(Grid);
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeydown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  FEdit.Parent := AParent;
End;

Procedure TCheckEditLink.DestroyEditor;
Begin
  FEdit.Free;
End;

Procedure TCheckEditLink.EditExit(sender: TObject);
Begin
  HideEditor;
End;

Function TCheckEditLink.GetEditControl: TWinControl;
Begin
  Result := FEdit;
End;


Function TCheckEditLink.GetEditorValue: String;
Begin
  Result := FEdit.Text;
End;

procedure TCheckEditLink.SetCellProps(AColor: TColor; AFont: TFont);
begin
  inherited;
  FEdit.Color := AColor;
end;

Procedure TCheckEditLink.SetEditorValue(s: String);
Begin
  FEdit.Text := s;
End;

{ TImagePickerEditLink }

Constructor TImagePickerEditLink.Create(aOwner: TComponent);
Begin
  Inherited;
  WantKeyUpDown := True;
  DropDownWidth := 100;
  DropDownHeight := 100;
  FEdit := TImagePicker.Create(Grid);
  FEdit.OnExit := EditExit;
  FEdit.OnKeydown := EditKeyDown;
  FEdit.Width := 0;
  FEdit.Height := 0;

End;

Procedure TImagePickerEditLink.CreateEditor(AParent: TWinControl);
Begin
  Inherited;
  FEdit.Parent := AParent;
  FEdit.Flat := True;
  FEdit.FlatLineColor := clNone;
  FEdit.Etched := True;
  FEdit.ItemHeight := 32;
End;

Procedure TImagePickerEditLink.DestroyEditor;
Begin
  FEdit.Free;
End;

procedure TImagePickerEditLink.EditExit(Sender: TObject);
Begin
  HideEditor;
End;

function TImagePickerEditLink.GetEditControl: TWinControl;
Begin
  Result := FEdit;
End;

function TImagePickerEditLink.GetEditorValue: String;
begin
  if FEdit.HandleAllocated then
  begin
    If FEdit.ItemIndex >= 0 Then
    begin
      Result := IntToStr(FEdit.Items.Items[FEdit.ItemIndex].ImageIndex);
    end;
  end;
end;

procedure TImagePickerEditLink.SetEditorValue(s: String);
begin
  if s = '' then
    s := '0';
  FEdit.SelectByImageIdx(StrToInt(s));
end;

procedure TImagePickerEditLink.SetProperties;
Begin
  Inherited;
  FEdit.Images := FImages;

End;

procedure TImagePickerEditLink.SetRect(r: trect);
Begin
  Inherited;
  FEdit.Height := r.Bottom - r.Top + FDropDownHeight;
  FEdit.DropHeight := FDropDownHeight;
  FEdit.DropWidth := FDropDownWidth;
  FEdit.EditHeight := r.Bottom - r.Top - 7;
End;


{ TAdvFileNameEditLink }

constructor TAdvFileNameEditLink.Create(AOwner: TComponent);
begin
  inherited;
  WantKeyLeftRight := True;
  WantKeyHomeEnd := True;
  EditColor := clWindow;
  ModifiedColor := clRed;
end;

procedure TAdvFileNameEditLink.CreateEditor(AParent: TWinControl);
begin
  FEdit := TAdvFileNameEdit.Create(Grid);
  FEdit.ShowModified := True;
  FEdit.DefaultHandling := False;
  FEdit.ModifiedColor := clRed;
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  WantKeyLeftRight := True;
  WantKeyHomeEnd := True;
  FEdit.Parent := AParent;
  FEdit.Color := EditColor;
end;

destructor TAdvFileNameEditLink.Destroy;
begin
  inherited;
end;

procedure TAdvFileNameEditLink.DestroyEditor;
begin
  If Assigned(FEdit) Then
    FEdit.Free;
  FEdit := Nil;
end;

procedure TAdvFileNameEditLink.EditExit(Sender: TObject);
begin
  HideEditor;
end;

function TAdvFileNameEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;

function TAdvFileNameEditLink.GetEditorValue: String;
begin
  Result := FEdit.Text;
end;

procedure TAdvFileNameEditLink.SetEditorValue(s: String);
begin
  FEdit.Text := s;
end;

procedure TAdvFileNameEditLink.SetProperties;
begin
  Inherited;
  FEdit.Color := FEditColor;
  FEdit.ModifiedColor := FModifiedColor;
  FEdit.ShowModified := FShowModified;

  FEdit.Filter := FFilter;
  FEdit.FilterIndex := FFilterIndex;
  FEdit.InitialDir := FInitialDir;
  FEdit.DialogOptions := FDialogOptions;
  FEdit.DialogTitle := FDialogTitle;
  FEdit.DialogKind := FDialogKind;
  FEdit.DefaultExt := FDefaultExt;
end;



{ TAdvDirectoryEditLink }

constructor TAdvDirectoryEditLink.Create(AOwner: TComponent);
begin
  inherited;
  WantKeyLeftRight := True;
  WantKeyHomeEnd := True;
  EditColor := clWindow;
  ModifiedColor := clRed;
end;

procedure TAdvDirectoryEditLink.CreateEditor(AParent: TWinControl);
begin
  FEdit := TAdvDirectoryEdit.Create(Grid);
  FEdit.ShowModified := True;
  FEdit.DefaultHandling := False;
  FEdit.ModifiedColor := clRed;
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnExit := EditExit;
  FEdit.Width := 0;
  FEdit.Height := 0;
  WantKeyLeftRight := True;
  WantKeyHomeEnd := True;
  FEdit.Parent := AParent;
  FEdit.Color := EditColor;
end;

destructor TAdvDirectoryEditLink.Destroy;
begin
  inherited;
end;

procedure TAdvDirectoryEditLink.DestroyEditor;
begin
  If Assigned(FEdit) Then
    FEdit.Free;
  FEdit := Nil;
end;

procedure TAdvDirectoryEditLink.EditExit(Sender: TObject);
begin
  HideEditor;
end;

function TAdvDirectoryEditLink.GetEditControl: TWinControl;
begin
  Result := FEdit;
end;

function TAdvDirectoryEditLink.GetEditorValue: String;
begin
  Result := FEdit.Text;
end;

procedure TAdvDirectoryEditLink.SetEditorValue(s: String);
begin
  FEdit.Text := s;
end;

procedure TAdvDirectoryEditLink.SetProperties;
begin
  inherited;
  FEdit.Color := FEditColor;
  FEdit.ModifiedColor := FModifiedColor;
  FEdit.ShowModified := FShowModified;
end;


//------------------------------------------------
constructor TLUComboEditLink.Create(AOwner: TComponent);
//------------------------------------------------
begin
  inherited;
  Combo := nil;
  FDropHeight := 250;
  FDropWidth := 300;
  FItems := TStringList.Create;
  FFlat:=true;
  WantKeyUpDown := True;
end;

//------------------------------------------------
procedure TLUComboEditLink.CreateEditor(AParent: TWinControl);
//------------------------------------------------
begin
  inherited;
  if not Assigned(Combo) then
  begin
    Combo := TLUCombo.Create(AParent);
    Combo.Parent := AParent;
    Combo.OnExit := EditExit;
    Combo.OnKeydown := EditKeyDown;
    Combo.FocusBorder := False;
  end;
end;
 
//------------------------------------------------
destructor TLUComboEditLink.Destroy;
//------------------------------------------------
begin
  FItems.Free;
  inherited;
end;
 
//------------------------------------------------
procedure TLUComboEditLink.EditExit(Sender: TObject);
//------------------------------------------------
begin
  HideEditor;
end;
 
//------------------------------------------------
function TLUComboEditLink.GetEditControl: TWinControl;
//------------------------------------------------
begin
  Result := Combo;
end;
 
//------------------------------------------------
function TLUComboEditLink.GetEditorValue: string;
//------------------------------------------------
begin
  result := Combo.text;
end;
 
//------------------------------------------------
procedure TLUComboEditLink.Notification(AComponent: TComponent;  AOperation: TOperation);
//------------------------------------------------
begin
   inherited;
end;
 
{$IFDEF ASG194}
//------------------------------------------------
procedure TLUComboEditLink.SetCellProps(AColor: TColor; AFont: TFont);
//------------------------------------------------
begin
  FCombo.Color := AColor;
  FCombo.Font := AFont;
end;
{$ENDIF}
 
//------------------------------------------------
procedure TLUComboEditLink.SetEditorValue(s: string);
//------------------------------------------------
begin
  Combo.Text := s;
end;
 
//------------------------------------------------
procedure TLUComboEditLink.SetItems(const Value: TStrings);
//------------------------------------------------
begin
  FItems.Assign(Value);
end;
 
//------------------------------------------------
procedure TLUComboEditLink.SetProperties;
//------------------------------------------------
var
  i: Integer;
begin
  inherited;
  Combo.Flat := FFlat;
  Combo.Etched := FEtched;
  Combo.Height := FDropHeight;
  Combo.DropWidth := FDropWidth;
  Combo.Flat := true;
 
  Combo.Items.Clear;
  for i := 1 to FItems.Count do
  begin
    Combo.Items.Add(fitems.Strings[i - 1]);
  end;
end;

//------------------------------------------------
procedure TLUComboEditLink.SetRect(r: TRect);
//------------------------------------------------
begin
  inherited;
  FCellHeight := r.Bottom - r.Top;
end;




end.
