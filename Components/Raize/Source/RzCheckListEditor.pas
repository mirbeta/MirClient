{===============================================================================
  RzCheckListEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzCheckListEditor
    Adds context menu to edit the list's check states, the list's tab stops, and
    the list's Align property.


  Modification History
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * The CheckList Editor has been redesigned to make it easier to manage the
      items and groups in the check list. The editor has also been enhanced to
      allow setting the ImageIndex and DisabledIndex values for each item, which
      are used to specify an image to be displayed next each item.
    * The CheckList Editor is now resizable.
    * When saving check list data in the CheckList Editor (in RS 2009 or later
      with Unicode support), and the check list contains non-ANSI characters,
      the file is saved as a UTF8 encoded text file. The CheckList Editor is
      able to load ANSI or UTF8 encoded text files.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added "Manual Tab Stops" and "Automatic Tab Stops" items to context menu
      for TRzCheckList.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Updated form to use custom framing editing controls and HotTrack style
      buttons, radio buttons, and check boxes.
    * Added Clear button.
    * Added Save button.
    * Added Convert to Group checkbox.
===============================================================================}

{$I RzComps.inc}

unit RzCheckListEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Forms,
  Controls,
  Classes,
  StdCtrls,
  Menus,
  RzChkLst,
  RzDesignEditors,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  RzLstBox,
  Dialogs,
  RzButton,
  RzRadChk,
  ExtCtrls,
  RzPanel, RzShellDialogs, Mask, RzEdit, RzLabel, ImgList, RzSpnEdt;

type
  TRzCheckListProperty = class( TPropertyEditor )
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;


  TRzCheckListEditor = class( TRzDefaultEditor )
  protected
    function CheckList: TRzCheckList;
    function AlignMenuIndex: Integer; override;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                             var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  TRzCheckListEditDlg = class(TForm)
    grpPreview: TRzGroupBox;
    lstPreview: TRzCheckList;
    btnAdd: TRzButton;
    btnEdit: TRzButton;
    btnDelete: TRzButton;
    btnMoveUp: TRzButton;
    btnMoveDown: TRzButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    btnOK: TRzButton;
    btnCancel: TRzButton;
    RzPanel1: TRzPanel;
    lblImageIndex: TRzLabel;
    lblDisabledIndex: TRzLabel;
    chkItemEnabled: TRzCheckBox;
    optItem: TRzRadioButton;
    optGroup: TRzRadioButton;
    RzToolbar1: TRzToolbar;
    ImageList1: TImageList;
    btnLoad: TRzButton;
    btnSave: TRzButton;
    btnClear: TRzButton;
    RzSpacer1: TRzSpacer;
    RzSpacer2: TRzSpacer;
    chkAllowGrayed: TRzCheckBox;
    spnImageIndex: TRzSpinEdit;
    spnDisabledIndex: TRzSpinEdit;
    RzSpacer3: TRzSpacer;
    procedure lstPreviewClick(Sender: TObject);
    procedure chkItemEnabledClick(Sender: TObject);
    procedure chkAllowGrayedClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure optItemClick(Sender: TObject);
    procedure optGroupClick(Sender: TObject);
    procedure spnImageIndexChange(Sender: TObject);
    procedure spnDisabledIndexChange(Sender: TObject);
  private
    procedure EnableButtons( Enable: Boolean );
    procedure EnableMoveButtons( Idx: Integer );
    procedure EnableItemControls;
  public
    procedure UpdateControls;
  end;


implementation

{$R *.dfm}

uses
  RzCommon,
  SysUtils,
  Buttons,
  RzCheckListItemForm;

resourcestring
  sRzDeleteCheckItem  = 'Delete "%s"?';
  sRzAddCheckItem     = 'Add';
  sRzEditCheckItem    = 'Edit';


{==================================}
{== TRzCheckListProperty Methods ==}
{==================================}


function TRzCheckListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [ paDialog ];
end;


function TRzCheckListProperty.GetValue: string;
begin
  FmtStr( Result, '(%s)', [ GetPropType^.Name ] );
end;


procedure TRzCheckListProperty.Edit;
var
  Component: TComponent;
  Dlg: TRzCheckListEditDlg;
  OwnerName: string;

  procedure CopyCheckList( Dest, Source: TRzCheckList );
  var
    I: Integer;
  begin
    Dest.AllowGrayed := Source.AllowGrayed;
    Dest.Items.Assign( Source.Items );
    Dest.Sorted := Source.Sorted;
    Dest.Font := Source.Font;
    Dest.GroupFont := Source.GroupFont;
    Dest.GroupColor := Source.GroupColor;
    Dest.UseGradients := Source.UseGradients;
    Dest.TabStops := Source.TabStops;
    Dest.Images := Source.Images;
    for I := 0 to Source.Items.Count - 1 do
    begin
      Dest.ItemEnabled[ I ] := Source.ItemEnabled[ I ];
      Dest.ItemState[ I ] := Source.ItemState[ I ];
      Dest.ItemImageIndex[ I ] := Source.ItemImageIndex[ I ];
      Dest.ItemDisabledIndex[ I ] := Source.ItemDisabledIndex[ I ];
    end;
  end;

begin
  Component := TComponent( GetComponent( 0 ) );

  Dlg := TRzCheckListEditDlg.Create( Application );
  try
    CopyCheckList( Dlg.lstPreview, TRzCheckList( GetComponent( 0 ) ) );

    if Component.Owner <> nil then
      OwnerName := Component.Owner.Name + '.'
    else
      OwnerName := '';
    Dlg.Caption := OwnerName + Component.Name + Dlg.Caption;
    Dlg.UpdateControls;

    if Dlg.ShowModal = mrOK then                          { Display Dialog Box }
    begin
      CopyCheckList( TRzCheckList( GetComponent( 0 ) ), Dlg.lstPreview );
      Designer.Modified;
    end;
  finally
    Dlg.Free;
  end;
end;



{================================}
{== TRzCheckListEditor Methods ==}
{================================}

function TRzCheckListEditor.CheckList: TRzCheckList;
begin
  Result := Component as TRzCheckList;
end;


function TRzCheckListEditor.GetVerbCount: Integer;
begin
  Result := 10;
end;


function TRzCheckListEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Edit Check List...';
    1: Result := '-';
    2: Result := 'Edit Tab Stops...';
    3: Result := 'Manual Tab Stops';
    4: Result := 'Automatic Tab Stops';
    5: Result := '-';
    6: Result := 'Set ImageList';
    7: Result := '-';
    8: Result := 'Align';
    9: Result := 'XP Colors';
  end;
end;


function TRzCheckListEditor.AlignMenuIndex: Integer;
begin
  Result := 8;
end;


function TRzCheckListEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                            var CompRefPropName: string;
                                            var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  if Index = 6 then
  begin
    CompRefClass := TCustomImageList;
    CompRefPropName := 'Images';
    CompRefMenuHandler := nil;
    Result := True;
  end
end;


procedure TRzCheckListEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    3: Item.Checked := CheckList.TabStopsMode = tsmManual;
    4: Item.Checked := CheckList.TabStopsMode = tsmAutomatic;
  end;
end;


procedure TRzCheckListEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    0: EditPropertyByName( 'Items' );
    2: EditPropertyByName( 'TabStops' );

    3: CheckList.TabStopsMode := tsmManual;
    4: CheckList.TabStopsMode := tsmAutomatic;
    7:
    begin
      CheckList.HighlightColor := xpRadChkMarkColor;
      CheckList.ItemFrameColor := xpRadChkFrameColor;
    end;
  end;
  if Index in [ 3, 4, 7 ] then
    DesignerModified;
end;


{=================================}
{== TRzCheckListEditDlg Methods ==}
{=================================}

procedure TRzCheckListEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
end;


procedure TRzCheckListEditDlg.UpdateControls;
begin
  chkAllowGrayed.Checked := lstPreview.AllowGrayed;
  EnableButtons( lstPreview.Items.Count > 0 );
  if lstPreview.Items.Count > 0 then
  begin
    lstPreview.ItemIndex := 0;
    EnableMoveButtons( 0 );
    EnableItemControls;
  end;
end;


procedure TRzCheckListEditDlg.EnableButtons( Enable: Boolean );
begin
  btnDelete.Enabled := Enable;
  btnEdit.Enabled := Enable;
  btnMoveUp.Enabled := Enable;
  btnMoveDown.Enabled := Enable;
end;


procedure TRzCheckListEditDlg.EnableMoveButtons( Idx: Integer );
begin
  btnMoveUp.Enabled := Idx > 0;
  btnMoveDown.Enabled := Idx < lstPreview.Items.Count - 1;
end;


procedure TRzCheckListEditDlg.EnableItemControls;
var
  Idx: Integer;
  ItemSelected: Boolean;
begin
  Idx := lstPreview.ItemIndex;
  ItemSelected := Idx <> -1;
  optItem.Enabled := ItemSelected;
  optGroup.Enabled := ItemSelected;

  chkItemEnabled.Enabled := ItemSelected and optItem.Checked;
  lblImageIndex.Enabled := ItemSelected and optItem.Checked;
  spnImageIndex.Enabled := ItemSelected and optItem.Checked;
  lblDisabledIndex.Enabled := ItemSelected and optItem.Checked;
  spnDisabledIndex.Enabled := ItemSelected and optItem.Checked;

  if ItemSelected then
  begin
    chkItemEnabled.Checked := lstPreview.ItemEnabled[ Idx ];
    if lstPreview.ItemIsGroup[ Idx ] then
      optGroup.Checked := True
    else
      optItem.Checked := True;
    spnImageIndex.IntValue := lstPreview.ItemImageIndex[ Idx ];
    spnDisabledIndex.IntValue := lstPreview.ItemDisabledIndex[ Idx ];
    EnableMoveButtons( Idx );
  end;
end;


procedure TRzCheckListEditDlg.lstPreviewClick(Sender: TObject);
begin
  EnableItemControls;
end;


procedure TRzCheckListEditDlg.optGroupClick(Sender: TObject);
begin
  lstPreview.ItemToGroup( lstPreview.ItemIndex );
  EnableItemControls;
end;


procedure TRzCheckListEditDlg.optItemClick(Sender: TObject);
begin
  lstPreview.GroupToItem( lstPreview.ItemIndex );
  EnableItemControls;
end;


procedure TRzCheckListEditDlg.spnDisabledIndexChange(Sender: TObject);
begin
  lstPreview.ItemDisabledIndex[ lstPreview.ItemIndex ] := spnDisabledIndex.IntValue;
end;


procedure TRzCheckListEditDlg.spnImageIndexChange(Sender: TObject);
begin
  lstPreview.ItemImageIndex[ lstPreview.ItemIndex ] := spnImageIndex.IntValue;
end;


procedure TRzCheckListEditDlg.chkItemEnabledClick(Sender: TObject);
begin
  lstPreview.ItemEnabled[ lstPreview.ItemIndex ] := chkItemEnabled.Checked;
end;


procedure TRzCheckListEditDlg.chkAllowGrayedClick(Sender: TObject);
begin
  lstPreview.AllowGrayed := chkAllowGrayed.Checked;
end;


procedure TRzCheckListEditDlg.btnAddClick(Sender: TObject);
var
  Dlg: TRzCheckItemEditDlg;
  Idx: Integer;
begin
  { add a new entry }
  Dlg := TRzCheckItemEditDlg.Create( Application );
  try
    Dlg.Caption := sRzAddCheckItem;
    Dlg.Item := '';
    if Dlg.ShowModal = mrOK then
    begin
      if Dlg.optGroup.Checked then
        Idx := lstPreview.AddGroup( Dlg.Item )
      else
        Idx := lstPreview.Items.Add( Dlg.Item );
      lstPreview.ItemIndex := Idx;
      EnableButtons( True );
      EnableItemControls;
    end;
  finally
    Dlg.Free;
  end;
end;


procedure TRzCheckListEditDlg.btnEditClick(Sender: TObject);
var
  Dlg: TRzCheckItemEditDlg;
  Idx: Integer;
begin
  Dlg := TRzCheckItemEditDlg.Create( Application );
  try
    Idx := lstPreview.ItemIndex;
    Dlg.Caption := sRzEditCheckItem;

    Dlg.Item := lstPreview.ItemCaption( Idx );
    if lstPreview.ItemIsGroup[ Idx ] then
      Dlg.optGroup.Checked := True;

    if Dlg.ShowModal = mrOK then
    begin
      if Dlg.optGroup.Checked then
        lstPreview.Items[ Idx ] := strDefaultGroupPrefix + Dlg.Item
      else
        lstPreview.Items[ Idx ] := Dlg.Item;

      lstPreview.ItemIndex := Idx;
    end;
  finally
    Dlg.Free;
  end;
end;


procedure TRzCheckListEditDlg.btnDeleteClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lstPreview.ItemIndex;
  if MessageDlg( Format( sRzDeleteCheckItem, [ lstPreview.Items[ Idx ] ] ),
                 mtConfirmation, [ mbYes, mbNo ], 0 ) = mrYes then
  begin
    lstPreview.Items.Delete( Idx );

    if lstPreview.Items.Count > 0 then
    begin
      if Idx = lstPreview.Items.Count then
        Dec( Idx );
      lstPreview.ItemIndex := Idx;
    end
    else
      EnableButtons( False );
    lstPreviewClick( nil );                     { Update Item Enabled Checkbox }
  end;
end; {= TRzCheckListEditDlg.btnDeleteClick =}


procedure TRzCheckListEditDlg.btnClearClick(Sender: TObject);
begin
  lstPreview.Clear;
  EnableButtons( False );
  EnableMoveButtons( -1 );
end;


procedure TRzCheckListEditDlg.btnMoveUpClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lstPreview.ItemIndex;
  lstPreview.Items.Move( Idx, Idx - 1 );
  lstPreview.ItemIndex := Idx - 1;
  EnableMoveButtons( Idx - 1 );
end;


procedure TRzCheckListEditDlg.btnMoveDownClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lstPreview.ItemIndex;
  lstPreview.Items.Move( Idx, Idx + 1 );
  lstPreview.ItemIndex := Idx + 1;
  EnableMoveButtons( Idx + 1 );
end;


procedure TRzCheckListEditDlg.btnLoadClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    lstPreview.LoadFromFile( dlgOpen.FileName );
    UpdateControls;
  end;
end;


procedure TRzCheckListEditDlg.btnSaveClick(Sender: TObject);
{$IFDEF UNICODE}
var
  UseANSI: Boolean;
{$ENDIF}
begin
  if dlgSave.Execute then
  begin
    {$IFDEF UNICODE}
    UseANSI := lstPreview.Items.Text = UnicodeString( AnsiString( lstPreview.Items.Text ) );

    if UseANSI then
      lstPreview.SaveToFile( dlgSave.FileName, TEncoding.Default )
    else
      lstPreview.SaveToFile( dlgSave.FileName, TEncoding.UTF8 );

    {$ELSE}
    lstPreview.SaveToFile( dlgSave.FileName );
    {$ENDIF}
  end;
end;


end.
