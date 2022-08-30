{*************************************************************************}
{ TMS Find Dialog component                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2012 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvFindDialogForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, Buttons, StdCtrls, ExtCtrls;

type
  TAdvFindDialogOption = (fdoCaseSensitive, fdoExpression, fdoCloseIfFound,
    fdoWholeWordOnly, fdoWrapAtEndOfFile, fdoContinueToNextFile, fdoMoreEnabled,
    fdoSetMarkerEnabled, fdoPreviousEnabled, fdoFindEnabled, fdoCloseEnabled, fdoMoreExpanded, fdoFindList, fdoDown);

  TAdvFindDialogVisibleOption = (fdovCaseSensitive, fdovExpression, fdovCloseIfFound,
    fdovWholeWordOnly, fdovWrapAtEndOfFile, fdovContinueToNextFile, fdovMore, fdovSetMarker, fdovPrevious, fdovFind,
    fdovClose, fdovFindCombo, fdovFindMemo, fdovDirection);

  TAdvFindDialogOptions = set of TAdvFindDialogOption;
  TAdvFindDialogVisibleOptions = set of TAdvFindDialogVisibleOption;

const
  DefaultOptions = [fdoMoreEnabled, fdoPreviousEnabled, fdoSetMarkerEnabled, fdoFindEnabled, fdoCloseEnabled, fdoDown];
  DefaultVisibleOptions = [fdovCaseSensitive, fdovExpression, fdovCloseIfFound,
    fdovWholeWordOnly, fdovWrapAtEndOfFile, fdovPrevious, fdovFind,
    fdovClose, fdovFindCombo, fdovFindMemo, fdovDirection];

type
  TAdvFindDialog = class;

  TFindEditChangeEvent = procedure(Sender: TObject; var AText: string) of object;

  TFindUILanguage = class(TPersistent)
  private
    FCloseIfFound: String;
    FFindWhat: String;
    FCaseSensitive: String;
    FWrapAtEndOfFile: String;
    FExpression: String;
    FMore: String;
    FSetMarker: String;
    FPrevious: String;
    FWholeWordOnly: String;
    FClose: String;
    FFind: String;
    FContinueToNextFile: String;
    FLess: String;
    FDirection: String;
    FDown: String;
    FUp: String;
    FFindCaption: string;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property FindCaption: string read FFindCaption write FFindCaption;
    property FindWhat: String read FFindWhat write FFindWhat;
    property CaseSensitive: String read FCaseSensitive write FCaseSensitive;
    property Expression: String read FExpression write FExpression;
    property CloseIfFound: String read FCloseIfFound write FCloseIfFound;
    property WholeWordOnly: String read FWholeWordOnly write FWholeWordOnly;
    property WrapAtEndOfFile: String read FWrapAtEndOfFile write FWrapAtEndOfFile;
    property ContinueToNextFile: String read FContinueToNextFile write FContinueToNextFile;
    property Find: String read FFind write FFind;
    property Previous: String read FPrevious write FPrevious;
    property SetMarker: String read FSetMarker write FSetMarker;
    property Close: String read FClose write FClose;
    property More: String read FMore write FMore;
    property Less: String read FLess write FLess;
    property Direction: String read FDirection write FDirection;
    property Up: String read FUp write FUp;
    property Down: String read FDown write FDown;
  end;

  TFindDialogForm = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvFindDialog = class(TComponent)
  private
    FFindText: String;
    FOnFind: TNotifyEvent;
    FOnSetMarker: TNotifyEvent;
    FOnFindPrevious: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOptions: TAdvFindDialogOptions;
    FFindList: TStringList;
    FUILanguage: TFindUILanguage;
    FVisibleOptions: TAdvFindDialogVisibleOptions;
    FOnFindEditChange: TFindEditChangeEvent;
    FAutoHistory: boolean;
    Frm: TFindDialogForm;
    FOnShow: TNotifyEvent;
    FBorderStyle: TFormBorderStyle;
    procedure SetUILanguage(const Value: TFindUILanguage);
  protected
    procedure DoShow(Sender: TObject);
    procedure DoClose(Sender: TObject);
    procedure DoFind(Sender: TObject);
    procedure DoFindPrevious(Sender: TObject);
    procedure DoSetMarker(Sender: TObject);
    procedure DoFindEditChange(Sender: TObject; var AText: string);
    function CreateDialogForm(AOwner: TComponent): TFindDialogForm; virtual;
    function InitDialog: TCustomForm; virtual;
  public
    procedure Execute; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Form: TFindDialogForm read Frm;
  published
    property AutoHistory: boolean read FAutoHistory write FAutoHistory default true;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle default bsDialog;
    property FindText: String read FFindText write FFindText;
    property FindList: TStringList read FFindList write FFindList;
    property OnFindEditChange: TFindEditChangeEvent read FOnFindEditChange write FOnFindEditChange;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnFindPrevious: TNotifyEvent read FOnFindPrevious write FOnFindPrevious;
    property OnSetMarker: TNotifyEvent read FOnSetMarker write FOnSetMarker;
    property Options: TAdvFindDialogOptions read FOptions write FOptions default DefaultOptions;
    property VisibleOptions: TAdvFindDialogVisibleOptions read FVisibleOptions write FVisibleOptions default DefaultVisibleOptions;
    property UILanguage: TFindUILanguage read FUILanguage write SetUILanguage;
  end;


  TFindDialogForm = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    SpeedButton1: TSpeedButton;
    PopupMenu1: TPopupMenu;
    abCharacter1: TMenuItem;
    NewLine1: TMenuItem;
    AnyCharacter1: TMenuItem;
    CharacterinRange1: TMenuItem;
    CharacternotinRange1: TMenuItem;
    BeginningofLine1: TMenuItem;
    EndofLine1: TMenuItem;
    aggedExpression1: TMenuItem;
    Or1: TMenuItem;
    N0or1matches1: TMenuItem;
    N1orMoreMatches1: TMenuItem;
    N0or1Matches2: TMenuItem;
    N1: TMenuItem;
    FindBtn: TButton;
    PrevBtn: TButton;
    Marker: TButton;
    CloseBtn: TButton;
    MoreBtn: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Label2: TLabel;
    Memo1: TMemo;
    DirGroup: TRadioGroup;
    procedure SpeedButton1Click(Sender: TObject);
    procedure abCharacter1Click(Sender: TObject);
    procedure NewLine1Click(Sender: TObject);
    procedure AnyCharacter1Click(Sender: TObject);
    procedure CharacterinRange1Click(Sender: TObject);
    procedure CharacternotinRange1Click(Sender: TObject);
    procedure BeginningofLine1Click(Sender: TObject);
    procedure EndofLine1Click(Sender: TObject);
    procedure aggedExpression1Click(Sender: TObject);
    procedure Or1Click(Sender: TObject);
    procedure N0or1matches1Click(Sender: TObject);
    procedure N1orMoreMatches1Click(Sender: TObject);
    procedure N0or1Matches2Click(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FindBtnClick(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);
    procedure MarkerClick(Sender: TObject);
    procedure MoreBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure DirGroupClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    { Private declarations }
    FOnFind: TNotifyEvent;
    FOnSetMarker: TNotifyEvent;
    FOnFindPrevious: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnFindEditChange: TFindEditChangeEvent;
    FDialog: TAdvFindDialog;
    function GetFindText: String;
    procedure SetFindText(const Value: String);
  protected
    { Protected declarations }
    procedure DoFindEditChange(var AText: string); virtual;
    procedure EnableButtons; virtual;
    procedure UpdateMoreButton(const AValue: string); virtual;
  public
    { Public declarations }
    property Dialog: TAdvFindDialog read FDialog write FDialog;
    property OnFindEditChange: TFindEditChangeEvent read FOnFindEditChange write FOnFindEditChange;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnFindPrevious: TNotifyEvent read FOnFindPrevious write FOnFindPrevious;
    property OnSetMarker: TNotifyEvent read FOnSetMarker write FOnSetMarker;
    property FindText: String read GetFindText write SetFindText;
  end;

var
  FindDialogForm: TFindDialogForm;
  More: Boolean;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TAdvFindDialog.Assign(Source: TPersistent);
begin
  if (Source is TAdvFindDialog) then
  begin
    Options := (Source as TAdvFindDialog).Options;
    VisibleOptions := (Source as TAdvFindDialog).VisibleOptions;
    UILanguage.Assign((Source as TAdvFindDialog).UILanguage);
    AutoHistory := (Source as TAdvFindDialog).AutoHistory;
    FindText := (Source as TAdvFindDialog).FindText;
    FindList.Assign((Source as TAdvFindDialog).FindList);
  end;
end;

//------------------------------------------------------------------------------

constructor TAdvFindDialog.Create(AOwner: TComponent);
begin
  inherited;
  FUILanguage := TFindUILanguage.Create(Self);
  FOptions := DefaultOptions;
  FBorderStyle := bsDialog;
  FVisibleOptions := DefaultVisibleOptions;
  FFindList := TStringList.Create;
  FAutoHistory := True;
end;

//------------------------------------------------------------------------------

destructor TAdvFindDialog.Destroy;
begin
  FUILanguage.Destroy;
  FFindList.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvFindDialog.DoClose(Sender: TObject);
begin
  if FAutoHistory then
  begin
    if FFindList.IndexOf(FindText) = -1 then
      FFindList.Add(FindText);
  end;

  if Assigned(OnClose) then
    OnClose(Sender);
end;

//------------------------------------------------------------------------------

procedure TAdvFindDialog.DoFind(Sender: TObject);
begin
  if Assigned(OnFind) then
    OnFind(Sender);
end;

//------------------------------------------------------------------------------

procedure TAdvFindDialog.DoFindEditChange(Sender: TObject; var AText: string);
begin
  if Assigned(FOnFindEditChange) then
    FOnFindEditChange(Self, AText);
end;

//------------------------------------------------------------------------------

procedure TAdvFindDialog.DoFindPrevious(Sender: TObject);
begin
  if Assigned(OnFindPrevious) then
    OnFindPrevious(Sender);
end;

//------------------------------------------------------------------------------

procedure TAdvFindDialog.DoSetMarker(Sender: TObject);
begin
  if Assigned(OnSetMarker) then
    OnSetMarker(Sender);
end;

//------------------------------------------------------------------------------

procedure TAdvFindDialog.DoShow(Sender: TObject);
begin
  if Assigned(OnShow) then
    OnShow(Self);
end;

//------------------------------------------------------------------------------

function TAdvFindDialog.CreateDialogForm(AOwner: TComponent): TFindDialogForm;
begin
  Result := TFindDialogForm.Create(AOwner);
end;

//------------------------------------------------------------------------------

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));

  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

//------------------------------------------------------------------------------

function TAdvFindDialog.InitDialog: TCustomForm;
var
  DialogUnits: TPoint;
begin
  frm := CreateDialogForm(Self);
  frm.Dialog := Self;
  frm.FindText := FindText;
  frm.OnClose := DoClose;
  frm.OnFind := DoFind;
  frm.OnShow := DoShow;
  frm.OnFindPrevious := DoFindPrevious;
  frm.OnSetMarker := DoSetMarker;
  frm.OnFindEditChange := DoFindEditChange;

  frm.CheckBox1.Checked := fdoCaseSensitive in Options;
  frm.CheckBox2.Checked := fdoExpression in Options;
  frm.CheckBox3.Checked := fdoCloseIfFound in Options;
  frm.CheckBox4.Checked := fdoWholeWordOnly in Options;
  frm.CheckBox5.Checked := fdoWrapAtEndOfFile in Options;
  frm.CheckBox6.Checked := fdoContinueToNextFile in Options;
  frm.FindBtn.Enabled := fdoFindEnabled in Options;
  frm.PrevBtn.Enabled := fdoPreviousEnabled in Options;
  frm.Marker.Enabled := fdoSetMarkerEnabled in Options;
  frm.CloseBtn.Enabled := fdoCloseEnabled in Options;
  frm.MoreBtn.Enabled := fdoMoreEnabled in Options;

  frm.CheckBox1.Visible := fdovCaseSensitive in VisibleOptions;
  frm.CheckBox2.Visible := fdovExpression in VisibleOptions;
  frm.CheckBox3.Visible := fdovCloseIfFound in VisibleOptions;
  frm.CheckBox4.Visible := fdovWholeWordOnly in VisibleOptions;
  frm.CheckBox5.Visible := fdovWrapAtEndOfFile in VisibleOptions;
  frm.CheckBox6.Visible := fdovContinueToNextFile in VisibleOptions;
  frm.FindBtn.Visible := fdovFind in VisibleOptions;
  frm.PrevBtn.Visible := fdovPrevious in VisibleOptions;
  frm.Marker.Visible := fdovSetMarker in VisibleOptions;
  frm.CloseBtn.Visible := fdovClose in VisibleOptions;
  frm.MoreBtn.Visible := fdovMore in VisibleOptions;

  frm.CheckBox1.Caption := UILanguage.CaseSensitive;
  frm.CheckBox2.Caption := UILanguage.Expression;
  frm.CheckBox3.Caption := UILanguage.CloseIfFound;
  frm.CheckBox4.Caption := UILanguage.WholeWordOnly;
  frm.CheckBox5.Caption := UILanguage.WrapAtEndOfFile;
  frm.CheckBox6.Caption := UILanguage.ContinueToNextFile;
  frm.FindBtn.Caption := UILanguage.Find;
  frm.PrevBtn.Caption := UILanguage.Previous;
  frm.Marker.Caption := UILanguage.SetMarker;
  frm.CloseBtn.Caption := UILanguage.Close;
  frm.MoreBtn.Caption := UILanguage.More;

  frm.Label1.Caption := UILanguage.FindWhat;
  frm.Label2.Caption := UILanguage.FindWhat;

  frm.DirGroup.Caption := UILanguage.Direction;
  frm.DirGroup.Items[0] := UILanguage.Down;
  frm.DirGroup.Items[1] := UILanguage.Up;

  if fdoFindList in Options then
    frm.ComboBox1.Style := csDropDownList;

  frm.ComboBox1.Items.Assign(FindList);

  frm.ComboBox1.Visible := fdovFindCombo in VisibleOptions;
  frm.Memo1.Visible := fdovFindMemo in VisibleOptions;
  frm.DirGroup.Visible := fdovDirection in VisibleOptions;

  if not (fdoDown in Options) then
    frm.DirGroup.ItemIndex := 1;

  DialogUnits := GetAveCharSize(frm.Canvas);

  if frm.DirGroup.Visible then
    frm.Height := MulDiv(123, DialogUnits.Y, 8)  // 200
  else
    frm.Height := MulDiv(90, DialogUnits.Y, 8); // 147

  frm.Caption := UILanguage.FindCaption;

  if not frm.DirGroup.Visible then
  begin
    frm.memo1.Top := frm.memo1.Top - 54;
    frm.Label2.Top := frm.Label2.Top - 54;
  end;

  Result := frm;
end;

//------------------------------------------------------------------------------

procedure TAdvFindDialog.Execute;
var
  frm: TFindDialogForm;
begin
  frm := InitDialog as TFindDialogForm;
  frm.BorderStyle := BorderStyle;
  frm.EnableButtons;
  frm.FormStyle := fsStayOnTop;
  frm.Show;
end;

//------------------------------------------------------------------------------

procedure TAdvFindDialog.SetUILanguage(const Value: TFindUILanguage);
begin
  if FUILanguage <> Value then
    FUILanguage.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TFindUILanguage }

procedure TFindUILanguage.Assign(Source: TPersistent);
begin
  if (Source is TFindUILanguage) then
  begin
    FFindCaption := (Source as TFindUILanguage).FindCaption;
    FFindWhat :=  (Source as TFindUILanguage).FindWhat;
    FCaseSensitive := (Source as TFindUILanguage).CaseSensitive;
    FExpression := (Source as TFindUILanguage).Expression;
    FCloseIfFound := (Source as TFindUILanguage).CloseIfFound;
    FWholeWordOnly := (Source as TFindUILanguage).WholeWordOnly;
    FWrapAtEndOfFile := (Source as TFindUILanguage).WrapAtEndOfFile;
    FContinueToNextFile := (Source as TFindUILanguage).ContinueToNextFile;
    FFind := (Source as TFindUILanguage).Find;
    FPrevious := (Source as TFindUILanguage).Previous;
    FSetMarker := (Source as TFindUILanguage).SetMarker;
    FClose := (Source as TFindUILanguage).Close;
    FMore := (Source as TFindUILanguage).More;
    FLess := (Source as TFindUILanguage).Less;
    FDirection := (Source as TFindUILanguage).Direction;
    FUp := (Source as TFindUILanguage).Up;
    FDown := (Source as TFindUILanguage).Down;
  end;
end;

//------------------------------------------------------------------------------

constructor TFindUILanguage.Create(AOwner: TComponent);
begin
  inherited Create;
  if(csDesigning in AOwner.ComponentState) and not
    ((csReading in AOwner.ComponentState) or (csLoading in AOwner.ComponentState)) then
  begin
    FFindCaption := 'Find';
    FCloseIfFound := 'Cl&ose if Found';
    FCaseSensitive := '&Case sensitive';
    FExpression := 'E&xpression';
    FWholeWordOnly := '&Whole word Only';
    FWrapAtEndOfFile := 'Wrap at the end of file';
    FContinueToNextFile := 'Continue to next file';
    FFind := '&Find';
    FFindWhat := 'Find what:';
    FSetMarker := 'Set &Marker';
    FPrevious := '&Previous';
    FClose := '&Close';
    FMore := 'Mor&e';
    FLess := 'L&ess';
    FDirection := '&Direction';
    FUp := 'Up';
    FDown := 'Down';
  end;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.abCharacter1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '\t';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.aggedExpression1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '()';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.AnyCharacter1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '.';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.BeginningofLine1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '^';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.FindBtnClick(Sender: TObject);
begin
  if Assigned(OnFind) then
    OnFind(Self);

  if CheckBox3.Checked then
    Close;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.PrevBtnClick(Sender: TObject);
begin
  if Assigned(OnFindPrevious) then
    OnFindPrevious(Self);
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.MarkerClick(Sender: TObject);
begin
  if Assigned(OnSetMarker) then
    OnSetMarker(Self);
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.Memo1Change(Sender: TObject);
var
  s: string;
begin
  s := Memo1.Lines.Text;
  DoFindEditChange(s);
  Dialog.FindText := s;
  EnableButtons;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.UpdateMoreButton(const AValue: string);
begin
  MoreBtn.Caption := AValue;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.MoreBtnClick(Sender: TObject);
var
  heightopen, heightclosed: integer;
  DialogUnits: TPoint;
begin
  DialogUnits := GetAveCharSize(Canvas);
  if DirGroup.Visible then
  begin
    heightopen := MulDiv(193, DialogUnits.Y, 8);
    heightclosed := MulDiv(123, DialogUnits.Y, 8);
  end
  else
  begin
    heightopen := MulDiv(158, DialogUnits.Y, 8);
    heightclosed := MulDiv(90, DialogUnits.Y, 8);
  end;

  if not More then
  begin
    UpdateMoreButton(Dialog.UILanguage.Less);
    Dialog.Options := Dialog.Options + [fdoMoreExpanded];
    Height := heightopen;
  end
  else
  begin
    UpdateMoreButton(Dialog.UILanguage.More);
    Dialog.Options := Dialog.Options - [fdoMoreExpanded];
    Height := heightclosed;
  end;

  Label1.Enabled := More;
  ComboBox1.Enabled := More;
  More := not More;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.CharacterinRange1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '[]';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.CharacternotinRange1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '[^]';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    Dialog.Options := Dialog.Options + [fdoCaseSensitive]
  else
    Dialog.Options := Dialog.Options - [fdoCaseSensitive];
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    Dialog.Options := Dialog.Options + [fdoExpression]
  else
    Dialog.Options := Dialog.Options - [fdoExpression];
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
    Dialog.Options := Dialog.Options + [fdoCloseIfFound]
  else
    Dialog.Options := Dialog.Options - [fdoCloseIfFound];
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
    Dialog.Options := Dialog.Options + [fdoWholeWordOnly]
  else
    Dialog.Options := Dialog.Options - [fdoWholeWordOnly];
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.CheckBox5Click(Sender: TObject);
begin
  if CheckBox5.Checked then
    Dialog.Options := Dialog.Options + [fdoWrapAtEndOfFile]
  else
    Dialog.Options := Dialog.Options - [fdoWrapAtEndOfFile];
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.CheckBox6Click(Sender: TObject);
begin
  if CheckBox6.Checked then
    Dialog.Options := Dialog.Options + [fdoContinueToNextFile]
  else
    Dialog.Options := Dialog.Options - [fdoContinueToNextFile];
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.ComboBox1Change(Sender: TObject);
var
  s: string;
begin
  s := ComboBox1.Text;
  DoFindEditChange(s);
  Dialog.FindText := s;
  EnableButtons;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.EnableButtons;
begin
  FindBtn.Enabled := (Dialog.FindText <> '') and (fdoFindEnabled in Dialog.Options);
  PrevBtn.Enabled := (Dialog.FindText <> '') and (fdoPreviousEnabled in Dialog.Options);
  Marker.Enabled := (Dialog.FindText <> '') and (fdoSetMarkerEnabled in Dialog.Options);
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.DirGroupClick(Sender: TObject);
begin
  if DirGroup.ItemIndex = 0 then
    Dialog.Options := Dialog.Options + [fdoDown]
  else
    Dialog.Options := Dialog.Options - [fdoDown];
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.DoFindEditChange(var AText: string);
begin
  if Assigned(FOnFindEditChange) then
    FOnFindEditChange(Self, AText);
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.EndofLine1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '$';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(OnClose) then
    OnClose(Self);
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.FormCreate(Sender: TObject);
begin
  More := False;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.FormShow(Sender: TObject);
begin
  if (fdoMoreExpanded in Dialog.Options) then
  begin
    if not More then
      MoreBtnClick(Self);
  end
  else
  begin
    if More then
      MoreBtnClick(Self);
  end;
end;

//------------------------------------------------------------------------------

function TFindDialogForm.GetFindText: String;
begin
  if not More then
    Result := ComboBox1.Text
  else
    Result := Memo1.Text;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.N0or1matches1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '*';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.N0or1Matches2Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '?';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.N1orMoreMatches1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '+';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.NewLine1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '\n';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.Or1Click(Sender: TObject);
begin
  ComboBox1.Text := ComboBox1.Text + '|';
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.SetFindText(const Value: String);
begin
  if not More then
    ComboBox1.Text := Value
  else
    Memo1.Text := Value;
end;

//------------------------------------------------------------------------------

procedure TFindDialogForm.SpeedButton1Click(Sender: TObject);
var
   p : TPoint;
begin
 with Sender as TSpeedButton do
 begin
    GetCursorPos(p);
    p.x := Left + 1;
    p.y := Top + Height + 1;
    p := Self.ClientToScreen(p);
    PopupMenu1.Popup(p.x, p.y);
  end;
end;

//------------------------------------------------------------------------------

end.
