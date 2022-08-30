unit StylesSimpleDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, cxGridLevel, cxControls, cxGridCommon,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  cxGrid, cxGridCardView, cxGridDBCardView, ComCtrls, ToolWin, ImgList,
  cxStyles, StdCtrls, ExtCtrls, Buttons, cxGridCustomPopupMenu, cxGridPopupMenu,
  Menus, ActnList, cxCustomData, cxGraphics, cxFilter, cxData, cxEdit, cxDBData,
  cxClasses, cxListBox, cxContainer, cxDataStorage, cxCalendar,
  cxDBLookupComboBox, cxLookAndFeels, cxLookAndFeelPainters, BaseForm,
  cxButtons, cxGroupBox;

type
  TStylesSimpleDemoMainForm = class(TfmBaseForm)
    lvPersons: TcxGridLevel;
    cxGrid: TcxGrid;
    tvPersons: TcxGridDBTableView;
    Splitter: TSplitter;
    tvPersonsID: TcxGridDBColumn;
    tvPersonsFIRSTNAME: TcxGridDBColumn;
    tvPersonsSECONDNAME: TcxGridDBColumn;
    tvPersonsBIRTHNAME: TcxGridDBColumn;
    tvPersonsDATEOFBIRTH: TcxGridDBColumn;
    tvPersonsBIRTHCOUNTRY: TcxGridDBColumn;
    tvPersonsLOCATIONOFBIRTH: TcxGridDBColumn;
    tvPersonsBIOGRAPHY: TcxGridDBColumn;
    cxGridPopupMenu: TcxGridPopupMenu;
    alMain: TActionList;
    actHeader: TAction;
    actFooter: TAction;
    miOptions: TMenuItem;
    miHeader: TMenuItem;
    miFooter: TMenuItem;
    ilMain: TImageList;
    actIndicator: TAction;
    actGroupBox: TAction;
    actPreview: TAction;
    miIndicator: TMenuItem;
    miGroupByBox: TMenuItem;
    miPreview: TMenuItem;
    gbChangeStyles: TcxGroupBox;
    memHowTo: TMemo;
    ListBox: TcxListBox;
    pnlButtons: TPanel;
    btnEdit: TcxButton;
    btnView: TcxButton;
    procedure FormCreate(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure actHeaderExecute(Sender: TObject);
    procedure actFooterExecute(Sender: TObject);
    procedure actIndicatorExecute(Sender: TObject);
    procedure actGroupBoxExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
  private
    function GetSelectedStyle: TcxStyle;
    procedure RestoreDefaults(Sender: TObject);
  end;

var
  StylesSimpleDemoMainForm: TStylesSimpleDemoMainForm;

implementation

{$R *.dfm}

uses
  StylesSimpleDemoData, StylesSimpleDemoEdit, StylesSimpleDemoAssign, AboutDemoForm;

procedure TStylesSimpleDemoMainForm.FormCreate(Sender: TObject);
var
  I: integer;
  AStyle: TcxStyle;
begin
  with ListBox, StylesSimpleDemoMainDM do
  begin
    Clear;
    for I := 0 to StyleRepository.Count -1  do
    begin
       AStyle := TcxStyle(StyleRepository[I]);
       Items.AddObject(AStyle.Name, AStyle);
    end;
    ItemIndex := 0;
  end;
end;

function TStylesSimpleDemoMainForm.GetSelectedStyle: TcxStyle;
begin
  Result := TcxStyle(ListBox.Items.Objects[ListBox.ItemIndex]);
end;

procedure TStylesSimpleDemoMainForm.btnEditClick(Sender: TObject);
begin
  ChangeStyle(GetSelectedStyle);
end;

procedure TStylesSimpleDemoMainForm.btnViewClick(Sender: TObject);
begin
  ChangeStyleBinding(RestoreDefaults);
end;

procedure TStylesSimpleDemoMainForm.RestoreDefaults(Sender: TObject);
begin
 with tvPersons.Styles do
  begin
    Background := nil;
    Content := nil;
    ContentEven := nil;
    ContentOdd := nil;
    Footer := nil;
    FilterBox := nil;
    Group := nil;
    GroupByBox := nil;
    Header := nil;
    Indicator := nil;
    Inactive := nil;
    IncSearch := nil;
    Selection := nil;
    Preview := nil;
    StyleSheet := StylesSimpleDemoMainDM.UserStyleSheet;
  end;
end;

procedure TStylesSimpleDemoMainForm.actHeaderExecute(Sender: TObject);
begin
  tvPersons.OptionsView.Header := not tvPersons.OptionsView.Header;
  TAction(Sender).Checked := tvPersons.OptionsView.Header;
end;

procedure TStylesSimpleDemoMainForm.actFooterExecute(Sender: TObject);
begin
  tvPersons.OptionsView.Footer := not tvPersons.OptionsView.Footer;
  TAction(Sender).Checked := tvPersons.OptionsView.Footer;
end;

procedure TStylesSimpleDemoMainForm.actIndicatorExecute(Sender: TObject);
begin
  tvPersons.OptionsView.Indicator := not tvPersons.OptionsView.Indicator;
  TAction(Sender).Checked := tvPersons.OptionsView.Indicator;
end;

procedure TStylesSimpleDemoMainForm.actGroupBoxExecute(Sender: TObject);
begin
  tvPersons.OptionsView.GroupByBox := not tvPersons.OptionsView.GroupByBox;
  TAction(Sender).Checked := tvPersons.OptionsView.GroupByBox;
end;

procedure TStylesSimpleDemoMainForm.actPreviewExecute(Sender: TObject);
begin
  tvPersons.Preview.Visible := not tvPersons.Preview.Visible;
  TAction(Sender).Checked := tvPersons.Preview.Visible;
end;

end.  



