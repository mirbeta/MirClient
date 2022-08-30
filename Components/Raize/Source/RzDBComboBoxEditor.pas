{===============================================================================
  RzDBComboBoxEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzDBComboBoxEditor
    Adds context menu to TRzDBComboBox to quickly add items and values.


  Modification History
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Initial release. Because the TRzDBComboBox now supports the Values
      property along with the Items property a new component editor has been
      created that makes it easy to add items and values to the control. Items
      and corresponding Values can be entered simulatenously into a grid.
      Alternatively, the user can click the Load button and specify a text file
      that contains the items. If the text file is structured in item=value
      format, both columns of the grid will be populated appropriately.
===============================================================================}

{$I RzComps.inc}

unit RzDBComboBoxEditor;

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
  DesignMenus,
  RzDBDesignEditors,
  RzPanel,
  Grids,
  RzDBCmbo,
  RzButton,
  RzGrids;


type
  {===========================================}
  {== TRzDBComboBoxEditor Class Declaration ==}
  {===========================================}

  TRzDBComboBoxEditor = class( TRzDBControlEditor )
  protected
    function ComboBox: TRzDBComboBox;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzDBComboBoxEditDlg = class(TForm)
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
    procedure UpdateUI( ComboBox: TRzDBComboBox );
    procedure UpdateCombo( ComboBox: TRzDBComboBox );
  end;


implementation

{$R *.dfm}

uses
  SysUtils,
  RzCommon;

{=================================}
{== TRzDBComboBoxEditor Methods ==}
{=================================}

function TRzDBComboBoxEditor.ComboBox: TRzDBComboBox;
begin
  // Helper function to provide quick access to component being edited.
  // Also makes sure Component is a TRzComboBox.
  Result := Component as TRzDBComboBox;
end;


function TRzDBComboBoxEditor.GetVerbCount: Integer;
begin
  Result := 11;
end;


function TRzDBComboBoxEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Edit Items...';
    4: Result := 'Edit Items && Values...';
    5: Result := '-';
    6: Result := 'csDropDown Style';
    7: Result := 'csDropDownList Style';
    8: Result := '-';
    9: Result := 'Sorted';
    10: Result := 'AllowEdit';
  end;
end;


procedure TRzDBComboBoxEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    6: Item.Checked := ComboBox.Style = csDropDown;
    7: Item.Checked := ComboBox.Style = csDropDownList;
    9: Item.Checked := ComboBox.Sorted;
    10: Item.Checked := ComboBox.AllowEdit;
  end;
end;


procedure TRzDBComboBoxEditor.ExecuteVerb( Index: Integer );
var
  D: TRzDBComboBoxEditDlg;
begin
  case Index of
    3: EditPropertyByName( 'Items' );

    4:
    begin
      D := TRzDBComboBoxEditDlg.Create( Application );
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

    6: ComboBox.Style := csDropDown;
    7: ComboBox.Style := csDropDownList;
    9: ComboBox.Sorted := not ComboBox.Sorted;
    10: ComboBox.AllowEdit := not ComboBox.AllowEdit;
  end;
  if Index in [ 6, 7, 9, 10 ] then
    DesignerModified;

end; {= TRzDBComboBoxEditor.ExecuteVerb =}



{==================================}
{== TRzDBComboBoxEditDlg Methods ==}
{==================================}

procedure TRzDBComboBoxEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;

  Icon.Handle := LoadIcon( HInstance, 'RZDESIGNEDITORS_EDIT_ICON' );

  Width := MulDiv( Width, Screen.PixelsPerInch, 96 );
  Height := MulDiv( Height, Screen.PixelsPerInch, 96 );
end;


procedure TRzDBComboBoxEditDlg.UpdateUI( ComboBox: TRzDBComboBox );
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


procedure TRzDBComboBoxEditDlg.UpdateCombo( ComboBox: TRzDBComboBox );
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


procedure TRzDBComboBoxEditDlg.btnLoadClick(Sender: TObject);
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
end; {= TRzDBComboBoxEditDlg.btnLoadClick =}


procedure TRzDBComboBoxEditDlg.ResetGrid;
begin
  grdItemsValues.Cols[ 1 ].Clear;
  grdItemsValues.Cols[ 2 ].Clear;
  grdItemsValues.Cells[ 1, 0 ] := 'Item';
  grdItemsValues.Cells[ 2, 0 ] := 'Value';
  grdItemsValues.RowCount := 1024;
end;


procedure TRzDBComboBoxEditDlg.btnClearClick(Sender: TObject);
begin
  ResetGrid;
end;


procedure TRzDBComboBoxEditDlg.grdItemsValuesResize(Sender: TObject);
var
  W: Integer;
begin
  W := ( grdItemsValues.ClientWidth - 10 - 1 ) div 3 - 1;
  grdItemsValues.ColWidths[ 0 ] := 10;
  grdItemsValues.ColWidths[ 1 ] := W * 2;
  grdItemsValues.ColWidths[ 2 ] := W;
end;

end.



