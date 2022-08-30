unit StylesSimpleDemoAssign;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxStyles;

type
  TStylesSimpleDemoAssignForm = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    Label4: TLabel;
    ComboBox4: TComboBox;
    Label5: TLabel;
    ComboBox5: TComboBox;
    Label6: TLabel;
    ComboBox6: TComboBox;
    Label7: TLabel;
    ComboBox7: TComboBox;
    Label8: TLabel;
    ComboBox8: TComboBox;
    Label9: TLabel;
    ComboBox9: TComboBox;
    Label10: TLabel;
    ComboBox10: TComboBox;
    btnRestore: TButton;
    Label11: TLabel;
    ComboBox11: TComboBox;
    Label12: TLabel;
    ComboBox12: TComboBox;
    Label14: TLabel;
    ComboBox14: TComboBox;
    Label13: TLabel;
    ComboBox13: TComboBox;
    procedure ComboBoxChange(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    RestoreDefaults: TNotifyEvent;
    function GetCurrentStyle(AGridItemID: integer): TcxStyle;
    procedure SetCurrentStyle(const AStyle: TcxStyle; AGridItemID: integer);
    procedure InitComboBox(AComboBox: TComboBox);
    procedure RefreshBinding;
  public
    { Public declarations }
  end;

  function ChangeStyleBinding(ACallback: TNotifyEvent): boolean;


implementation

uses
{$IFDEF CLR}
  Variants,
{$ENDIF}
  StylesSimpleDemoMain, StylesSimpleDemoData;

{$R *.dfm}
type
  TcxStyles = (sContent, sContentEven, sContentOdd, sBackground, sFilterBox, sFooter, sGroup,
  sGroupByBox, sHeader, sIndicator, sInactive, sIncSearch, sPreview, sSelection);

function ChangeStyleBinding(ACallback: TNotifyEvent): boolean;
begin
  with TStylesSimpleDemoAssignForm.Create(Application) do
  try
    RestoreDefaults := ACallback;
    Result := ShowModal = mrOK;
  finally
    Free;
  end;
end;


function TStylesSimpleDemoAssignForm.GetCurrentStyle(AGridItemID: integer): TcxStyle;
begin
  Result := nil;
  with StylesSimpleDemoMainForm do
  case TcxStyles(AGridItemID) of
    sContent:
      Result := tvPersons.Styles.Content;
    sContentEven:
      Result := tvPersons.Styles.ContentEven;
    sContentOdd:
      Result := tvPersons.Styles.ContentOdd;
    sBackground:
      Result := tvPersons.Styles.Background;
    sFooter:
      Result := tvPersons.Styles.Footer;
    sFilterBox:
      Result := tvPersons.Styles.FilterBox;
    sGroup:
      Result := tvPersons.Styles.Group;
    sGroupByBox:
      Result := tvPersons.Styles.GroupByBox;
    sHeader:
      Result := tvPersons.Styles.Header;
    sIndicator:
      Result := tvPersons.Styles.Indicator;
    sInactive:
      Result := tvPersons.Styles.Inactive;
    sIncSearch:
      Result := tvPersons.Styles.IncSearch;
    sPreview:
      Result := tvPersons.Styles.Preview;
    sSelection:
       Result := tvPersons.Styles.Selection;
  end;
end;

procedure TStylesSimpleDemoAssignForm.InitComboBox(AComboBox: TComboBox);
var
  I: integer;
  s, CurrentStyle: string;
begin
  with AComboBox, StylesSimpleDemoMainDM do
  begin
    Clear;
    CurrentStyle := '';
    for I := 0 to StyleRepository.Count - 1 do
    begin
      s := StyleRepository.Items[I].Name;
      Items.AddObject(s, StyleRepository.Items[I]);
      if StyleRepository.Items[I] = GetCurrentStyle(AComboBox.Tag) then
        CurrentStyle := s;
    end;
    ItemIndex := Items.IndexOf(CurrentStyle);
 end;
end;

procedure TStylesSimpleDemoAssignForm.ComboBoxChange(Sender: TObject);
begin
  with TComboBox(Sender) do
    SetCurrentStyle(TcxStyle(Items.Objects[ItemIndex]), Tag);
end;

procedure TStylesSimpleDemoAssignForm.SetCurrentStyle(const AStyle: TcxStyle; AGridItemID: integer);
begin
  with StylesSimpleDemoMainForm do
    case TcxStyles(AGridItemID) of
      sContent:
        tvPersons.Styles.Content := AStyle;
      sContentEven:
        tvPersons.Styles.ContentEven := AStyle;
      sContentOdd:
        tvPersons.Styles.ContentOdd := AStyle;
      sBackground:
        tvPersons.Styles.Background := AStyle;
      sFooter:
        tvPersons.Styles.Footer := AStyle;
      sFilterBox:
        tvPersons.Styles.FilterBox := AStyle;
      sGroup:
        tvPersons.Styles.Group := AStyle;
      sGroupByBox:
        tvPersons.Styles.GroupByBox := AStyle;
      sHeader:
        tvPersons.Styles.Header := AStyle;
      sIndicator:
        tvPersons.Styles.Indicator := AStyle;
      sInactive:
        tvPersons.Styles.Inactive := AStyle;
      sIncSearch:
        tvPersons.Styles.IncSearch := AStyle;
      sPreview:
        tvPersons.Styles.Preview := AStyle;
      sSelection:
        tvPersons.Styles.Selection := AStyle;
    end;
end;

procedure TStylesSimpleDemoAssignForm.btnRestoreClick(Sender: TObject);
begin
  if Assigned(RestoreDefaults) then RestoreDefaults(Sender);
  RefreshBinding;
end;


procedure TStylesSimpleDemoAssignForm.RefreshBinding;
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TComboBox then
      InitComboBox(TComboBox(Components[I]));
end;

procedure TStylesSimpleDemoAssignForm.FormCreate(Sender: TObject);
begin
  Position := poScreenCenter;
  RefreshBinding;
end;

end.
