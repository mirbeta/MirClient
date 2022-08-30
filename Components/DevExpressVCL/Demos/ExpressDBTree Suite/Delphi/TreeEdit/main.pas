unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, Grids, DBGrids, ExtCtrls, ComCtrls, StdCtrls,
  DBCtrls, Buttons, ShellAPI, dxdbtrel, dxmdaset;

type
  TFMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    DBTreeViewEdit1: TdxDBTreeViewEdit;
    DBNavigator1: TDBNavigator;
    BitBtn1: TBitBtn;
    Label1: TLabel;
    mdCustomer: TdxMemData;
    mdCustomerCustNo: TFloatField;
    mdCustomerCompany: TStringField;
    mdCustomerAddr1: TStringField;
    mdCustomerAddr2: TStringField;
    mdCustomerCity: TStringField;
    mdCustomerState: TStringField;
    mdCustomerZip: TStringField;
    mdCustomerCountry: TStringField;
    mdCustomerPhone: TStringField;
    mdCustomerFAX: TStringField;
    mdCustomerTaxRate: TFloatField;
    mdCustomerContact: TStringField;
    mdCustomerLastInvoiceDate: TDateTimeField;
    procedure DBTreeViewEdit1CloseUp(Sender: TObject; Accept: Boolean);
    procedure FormCreate(Sender: TObject);
  end;

var
  FMain: TFMain;

implementation

{$R *.DFM}

{Fill TreeNodes}
procedure TFMain.DBTreeViewEdit1CloseUp(Sender: TObject; Accept: Boolean);
Var
  tr : TTreeNode;
begin
  tr := DBTreeViewEdit1.Selected;
  if(Accept) then begin
   mdCustomer.FindField('state').AsString := '';  
    while (tr <> Nil) And (tr.Parent <> Nil) do begin
      tr := tr.Parent;
      if(tr.Level = 1) then
        mdCustomer.FindField('state').AsString := tr.Text;
      if(tr.Level = 0) then
        mdCustomer.FindField('country').AsString := tr.Text;
    end;
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
var
  tr, tr1 : TTreeNode;
begin
  mdCustomer.LoadFromBinaryFile('..\..\Data\Customer.dat');
  mdCustomer.Open;
  mdCustomer.DisableControls;
  DBTreeViewEdit1.Items.Clear;
  while Not mdCustomer.EOF do begin
    tr := DBTreeViewEdit1.GetTreeNodeByText(Nil, mdCustomer.FindField('country').AsString, False);
    if(tr = Nil) then
      tr := DBTreeViewEdit1.Items.Insert(Nil,mdCustomer.FindField('country').AsString);
    if(mdCustomer.FindField('state').AsString <> '') then begin
      tr1 := DBTreeViewEdit1.GetTreeNodeByText(tr, mdCustomer.FindField('state').AsString, False);
      if(tr1 = Nil) then
        tr1 := DBTreeViewEdit1.Items.AddChild(tr, mdCustomer.FindField('state').AsString);
      tr := tr1;
    end;
    tr1 := DBTreeViewEdit1.GetTreeNodeByText(tr, mdCustomer.FindField('city').AsString, False);
    if(tr1 = Nil) then
      DBTreeViewEdit1.Items.AddChild(tr, mdCustomer.FindField('city').AsString);
    mdCustomer.Next;
  end;
  mdCustomer.First;
  mdCustomer.EnableControls;
  DBTreeViewEdit1.TreeViewSortType := stText;
end;

end.
