unit ProviderModeDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,  Forms, Dialogs, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxEdit, ActnList, ImgList,
  Menus, cxClasses, cxControls, ComCtrls, StdCtrls, ShellAPI, ToolWin, cxDataStorage,
  cxInplaceContainer, cxVGrid, DemoBasicMain, cxLookAndFeels, cxNavigator,
  cxLookAndFeelPainters;

type
  TProviderModeDemoMainForm = class(TDemoBasicMainForm)
    StyleRepository: TcxStyleRepository;
    cxVirtualVerticalGrid: TcxVirtualVerticalGrid;
    cxVerticalGridStyleSheetDevExpress: TcxVerticalGridStyleSheet;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    GridLines1: TMenuItem;
    Node1: TMenuItem;
    Horizontal1: TMenuItem;
    Vertical1: TMenuItem;
    Both1: TMenuItem;
    miSeparator1: TMenuItem;
    cxNavigator1: TcxNavigator;
    actCellAutoHeight: TAction;
    CellAutoHeight1: TMenuItem;
    actCellEndEllipsis: TAction;
    CellEndEllipsis1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure miLinesClick(Sender: TObject);
    procedure actCellAutoHeightExecute(Sender: TObject);
    procedure actCellEndEllipsisExecute(Sender: TObject);
  private
    procedure GenerateColumns;
    procedure LoadData;
    procedure SaveData;
  end;

var
  ProviderModeDemoMainForm: TProviderModeDemoMainForm;

implementation

uses ProviderModeDemoClasses;

{$R *.dfm}

const
  TabChar = #9;

var
  CustomerList : TCustomerList;

procedure TProviderModeDemoMainForm.GenerateColumns;
begin
  with cxVirtualVerticalGrid do
  begin
    ClearRows;

    with AddChild(nil, TcxEditorRow) as TcxEditorRow do
    begin
      Properties.Caption := 'ID';
      Properties.DataBinding.ValueTypeClass := TcxIntegerValueType;
    end;

    with AddChild(nil, TcxEditorRow) as TcxEditorRow do
    begin
      Properties.Caption := 'Customer';
      Properties.DataBinding.ValueTypeClass := TcxStringValueType;
    end;

    with AddChild(nil, TcxEditorRow) as TcxEditorRow do
    begin
      Properties.Caption := 'Company';
      Properties.DataBinding.ValueTypeClass := TcxStringValueType;
    end;
  end;
end;

procedure TProviderModeDemoMainForm.LoadData;
var
  ACustomer: TCustomer;
  I: Integer;
  s: string;
begin
   CustomerList := TCustomerList.Create;
   with TStringList.Create do
   try
     LoadFromFile('contacts.txt');
     for I := 0 to Count - 1 do
     begin
       ACustomer := TCustomer.Create(CustomerList.NextID);
       s := Strings[I];
       ACustomer.Name := Copy(s, 1, Pos(TabChar, s) - 1);
       ACustomer.Description := Copy(s, Pos(TabChar, s) + 1, Length(s));
       CustomerList.Add(ACustomer);
     end;
   finally
     Free;
   end;
end;

procedure TProviderModeDemoMainForm.SaveData;
var
  ACustomer: TCustomer;
  I: Integer;
begin
  with TStringList.Create do
  try
    for I := 0 to CustomerList.Count - 1 do
    begin
       ACustomer := CustomerList.Customers[I];
       Add(ACustomer.Name + TabChar + ACustomer.Description);
    end;
    SaveToFile('contacts.txt');
  finally
    Free;
  end;
end;

procedure TProviderModeDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  LoadData;
  GenerateColumns;
  cxVirtualVerticalGrid.DataController.CustomDataSource := TCustomerDataSource.Create(CustomerList);
end;

procedure TProviderModeDemoMainForm.FormDestroy(Sender: TObject);
begin
  cxVirtualVerticalGrid.DataController.CustomDataSource.Free;
  CustomerList.Free;
end;

procedure TProviderModeDemoMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  I: Integer;
begin
  I := -1;
  if TCustomerDataSource(cxVirtualVerticalGrid.DataController.CustomDataSource).Modified then
    I := MessageDlg('Do you want to save the changes ?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  case I of
    mrYes: SaveData;
    mrCancel: CanClose := False;
  end;
end;

procedure TProviderModeDemoMainForm.miLinesClick(Sender: TObject);
begin
  with cxVirtualVerticalGrid.OptionsView do
    case TMenuItem(Sender).Tag of
      0: GridLines := vglNone;
      1: GridLines := vglHorizontal;
      2: GridLines := vglVertical;
      3: GridLines := vglBoth;
    end;
  TMenuItem(Sender).Checked := True;
end;

procedure TProviderModeDemoMainForm.actCellAutoHeightExecute(
  Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  cxVirtualVerticalGrid.OptionsView.CellAutoHeight := TAction(Sender).Checked;
end;

procedure TProviderModeDemoMainForm.actCellEndEllipsisExecute(
  Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  cxVirtualVerticalGrid.OptionsView.CellEndEllipsis := TAction(Sender).Checked;
end;

end.
