{===============================================================================
  RzComboBoxEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzComboBoxEditor 
    Adds context menu to TRzComboBox to quickly add items and values.


  Modification History
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Initial release. Because the TRzComboBox now supports the Values property
      along with the Items property a new component editor has been created
      that makes it easy to add items and values to the control. Items and
      corresponding Values can be entered simulatenously into a grid.
      Alternatively, the user can click the Load button and specify a text file
      that contains the items. If the text file is structured in item=value
      format, both columns of the grid will be populated appropriately.
===============================================================================}

{$I RzComps.inc}

unit RzComboBoxEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Windows,
  Controls,
  Graphics,
  Forms,
  Menus,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Classes,
  Dialogs,
  DesignIntf,
  DesignEditors,
  VCLEditors,
  DesignMenus,
  RzDesignEditors,
  RzPanel,
  Grids,
  RzCmboBx,
  RzButton, 
  RzGrids;


type
  {=========================================}
  {== TRzComboBoxEditor Class Declaration ==}
  {=========================================}

  TRzComboBoxEditor = class( TRzDefaultEditor )
  protected
    function ComboBox: TRzComboBox;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {===============================================}
  {== TRzComboBoxTextProperty Class Declaration ==}
  {===============================================}

  TRzComboBoxTextProperty = class( TCaptionProperty )
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues( Proc: TGetStrProc ); override;
    procedure SetValue( const Value: string ); override;
  end;


  TRzComboBoxEditDlg = class(TForm)
    PnlButtons: TRzPanel;
    btnLoad: TRzButton;
    btnClear: TRzButton;
    dlgOpen: TOpenDialog;
    RzPanel1: TRzPanel;
    btnOk: TRzButton;
    btnCancel: TRzButton;
    pnlClientArea: TRzPanel;
    grdItemsValues: TRzStringGrid;
    procedure btnLoadClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure grdItemsValuesResize(Sender: TObject);
  private
    procedure ResetGrid;
  public
    procedure UpdateUI( ComboBox: TRzComboBox );
    procedure UpdateCombo( ComboBox: TRzComboBox );
  end;


implementation

{$R *.dfm}

uses
  SysUtils,
  RzCommon;

{===============================}
{== TRzComboBoxEditor Methods ==}
{===============================}

function TRzComboBoxEditor.ComboBox: TRzComboBox;
begin
  // Helper function to provide quick access to component being edited.
  // Also makes sure Component is a TRzComboBox.
  Result := Component as TRzComboBox;
end;


function TRzComboBoxEditor.GetVerbCount: Integer;
begin
  Result := 8;
end;


function TRzComboBoxEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit Items...';
    1: Result := 'Edit Items && Values...';
    2: Result := '-';
    3: Result := 'csDropDown Style';
    4: Result := 'csDropDownList Style';
    5: Result := '-';
    6: Result := 'Sorted';
    7: Result := 'AllowEdit';
  end;
end;


procedure TRzComboBoxEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    3: Item.Checked := ComboBox.Style = csDropDown;
    4: Item.Checked := ComboBox.Style = csDropDownList;
    6: Item.Checked := ComboBox.Sorted;
    7: Item.Checked := ComboBox.AllowEdit;
  end;
end;


procedure TRzComboBoxEditor.ExecuteVerb( Index: Integer );
var
  D: TRzComboBoxEditDlg;
begin
  case Index of
    0: EditPropertyByName( 'Items' );

    1:
    begin
      D := TRzComboBoxEditDlg.Create( Application );
      try
        D.UpdateUI( ComboBox );

        // Set the dialog's Caption to reflect component being edited
        D.Caption := Component.Owner.Name +'.'+ Component.Name + D.Caption;

        if D.ShowModal = mrOK then
        begin
          D.UpdateCombo( ComboBox );

          // Tell the Form Designer to set the Modified flag for the form
          DesignerModified;
        end;
      finally
        D.Free;
      end;
    end;

    3: ComboBox.Style := csDropDown;
    4: ComboBox.Style := csDropDownList;
    6: ComboBox.Sorted := not ComboBox.Sorted;
    7: ComboBox.AllowEdit := not ComboBox.AllowEdit;
  end;
  if Index in [ 3, 4, 6, 7 ] then
    DesignerModified;

end; {= TRzComboBoxEditor.ExecuteVerb =}


{=====================================}
{== TRzComboBoxTextProperty Methods ==}
{=====================================}

function TRzComboBoxTextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [ paValueList ];
end;


procedure TRzComboBoxTextProperty.GetValues( Proc: TGetStrProc );
var
  I: Integer;
  C: TRzComboBox;
begin
  C := GetComponent( 0 ) as TRzComboBox;
  for I := 0 to C.Items.Count - 1 do
    Proc( C.Items[ I ] );
end;


procedure TRzComboBoxTextProperty.SetValue( const Value: string );
var
  C: TRzComboBox;
  Idx: Integer;
begin
  C := GetComponent( 0 ) as TRzComboBox;
  if C.Style <> csDropDown then
  begin
    Idx := C.Items.IndexOf( Value );
    if Idx <> -1 then
      C.ItemIndex := Idx;
  end;
  inherited;
end;





{================================}
{== TRzComboBoxEditDlg Methods ==}
{================================}

procedure TRzComboBoxEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;

  Icon.Handle := LoadIcon( HInstance, 'RZDESIGNEDITORS_EDIT_ICON' );

  Width := MulDiv( Width, Screen.PixelsPerInch, 96 );
  Height := MulDiv( Height, Screen.PixelsPerInch, 96 );
end;


procedure TRzComboBoxEditDlg.UpdateUI( ComboBox: TRzComboBox );
var
  I: Integer;
begin
  ResetGrid;
  grdItemsValues.RowCount := Max( ComboBox.Items.Count + 1,
                                  grdItemsValues.RowCount );

  for I := 0 to ComboBox.Items.Count - 1 do
    grdItemsValues.Cells[ 1, I + 1 ] := ComboBox.Items[ I ];

  for I := 0 to ComboBox.Values.Count - 1 do
    grdItemsValues.Cells[ 2, I + 1 ] := ComboBox.Values[ I ];
end;


procedure TRzComboBoxEditDlg.UpdateCombo( ComboBox: TRzComboBox );
var
  I: Integer;
  ItemStr: string;
begin
  ComboBox.Items.Clear;
  ComboBox.Values.Clear;
  for I := 1 to grdItemsValues.RowCount - 1 do
  begin
    ItemStr := Trim( grdItemsValues.Cells[ 1, I ] );
    if ItemStr <> '' then
    begin
      ComboBox.AddItemValue( ItemStr, Trim( grdItemsValues.Cells[ 2, I ] ) );
    end
    else
    begin
      // At first blank item, then break out of loop;
      Break;
    end;
  end;
end;


procedure TRzComboBoxEditDlg.btnLoadClick(Sender: TObject);
var
  FileContents: TStringList;
  S: string;
  I: Integer;
begin
  if dlgOpen.Execute then
  begin
    FileContents := TStringList.Create;
    try
      FileContents.LoadFromFile( dlgOpen.FileName );

      ResetGrid;
      grdItemsValues.RowCount := Max( FileContents.Count + 1,
                                      grdItemsValues.RowCount );

      for I := 0 to FileContents.Count - 1 do
      begin
        S := FileContents.Names[ I ];
        if S <> '' then
        begin
          grdItemsValues.Cells[ 1, I + 1 ] := S;
          grdItemsValues.Cells[ 2, I + 1 ] := FileContents.ValueFromIndex[ I ];
        end
        else
          grdItemsValues.Cells[ 1, I + 1 ] := FileContents[ I ];
      end;
    finally
      FileContents.Free;
    end;

  end;
end; {= TRzComboBoxEditDlg.btnLoadClick =}


procedure TRzComboBoxEditDlg.ResetGrid;
begin
  grdItemsValues.Cols[ 1 ].Clear;
  grdItemsValues.Cols[ 2 ].Clear;
  grdItemsValues.Cells[ 1, 0 ] := 'Item';
  grdItemsValues.Cells[ 2, 0 ] := 'Value';
  grdItemsValues.RowCount := 1024;
end;


procedure TRzComboBoxEditDlg.btnClearClick(Sender: TObject);
begin
  ResetGrid;
end;


procedure TRzComboBoxEditDlg.grdItemsValuesResize(Sender: TObject);
var
  W: Integer;
begin
  W := ( grdItemsValues.ClientWidth - 10 - 1 ) div 3 - 1;
  grdItemsValues.ColWidths[ 0 ] := 10;
  grdItemsValues.ColWidths[ 1 ] := W * 2;
  grdItemsValues.ColWidths[ 2 ] := W;
end;

end.



