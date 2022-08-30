{===============================================================================
  RzCheckListItemForm Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzCheckItemEditDlg
    Used by the TRzCheckListEditor dialog when adding/editing list items.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Updated form to use custom framing editing controls and HotTrack style
      buttons, radio buttons, and check boxes.
    * Added As Group check box to allow adding/editing groups.
===============================================================================}

{$I RzComps.inc}

unit RzCheckListItemForm;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Forms,
  StdCtrls,
  Controls,
  Classes,
  RzEdit,
  RzButton,
  RzLabel, RzRadChk;

type
  TRzCheckItemEditDlg = class(TForm)
    Label1: TRzLabel;
    btnOK: TRzButton;
    btnCancel: TRzButton;
    edtItem: TRzMemo;
    optItem: TRzRadioButton;
    optGroup: TRzRadioButton;
    procedure FormCreate(Sender: TObject);
  private
    procedure SetItem( const Item: string );
    function GetItem: string;
  public
    property Item: string
      read GetItem
      write SetItem;
  end;


implementation

{$R *.dfm}

procedure TRzCheckItemEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
end;


procedure TRzCheckItemEditDlg.SetItem( const Item: string );
begin
  edtItem.Text := Item;
  edtItem.SelectAll;
end;


function TRzCheckItemEditDlg.GetItem: string;
begin
  Result := edtItem.Text;
end;


end.
