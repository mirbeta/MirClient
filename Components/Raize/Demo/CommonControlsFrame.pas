{=======================================================================================================================
  CommonControlsFrame Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

unit CommonControlsFrame;

interface

uses
  Forms,
  ImgList,
  Controls,
  Menus,
  RzButton,
  RzRadChk,
  StdCtrls,
  RzLabel,
  RzPanel,
  RzEdit,
  ComCtrls,
  RzListVw,
  RzTreeVw,
  RzSplit,
  RzStatus,
  Classes,
  ExtCtrls,
  RzCommon,
  RzBorder;

type
  TFmeCommonControls = class(TFrame)
    RzSplitter1: TRzSplitter;
    tvwFolders: TRzCheckTree;
    RzSplitter2: TRzSplitter;
    edtMessage: TRzMemo;
    ImlTreeView: TImageList;
    lvwEmail: TRzListView;
    RzPanel1: TRzPanel;
    RzLabel1: TRzLabel;
    RzLabel2: TRzLabel;
    RzLabel3: TRzLabel;
    lblFrom: TRzLabel;
    RzLabel5: TRzLabel;
    lblSubject: TRzLabel;
    RzMenuController1: TRzMenuController;
    pnlHeader: TRzPanel;
    RzPanel3: TRzPanel;
    RzPanel4: TRzPanel;
    ChkCascadeChecks: TRzCheckBox;
    ChkAlphaSortAll: TRzCheckBox;
    ChkFillLastCol: TRzCheckBox;
    procedure lvwEmailChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ChkCascadeChecksClick(Sender: TObject);
    procedure ChkAlphaSortAllClick(Sender: TObject);
    procedure ChkFillLastColClick(Sender: TObject);
  private
    procedure PopulateListView;
    procedure PopulateTreeView;
  public
    procedure Init;
    procedure UpdateVisualStyle( VS: TRzVisualStyle; GCS: TRzGradientColorStyle );
  end;


implementation

{$R *.dfm}

uses
  Dialogs;


procedure TFmeCommonControls.Init;
begin
//  ParentBackground := False;

  PopulateListView;
  PopulateTreeView;

  tvwFolders.Items[ 0 ].Expand( True );
  
end;


procedure TFmeCommonControls.PopulateListView;
var
  Item: TListItem;
begin
  Item := lvwEmail.Items.Add;
  Item.Caption := 'Jennifer Davis';
  Item.SubItems.Add( 'Congratulations on Konopka Signature VCL Controls' );
  Item.ImageIndex := 8;

  Item := lvwEmail.Items.Add;
  Item.Caption := 'Arthur Jones';
  Item.SubItems.Add( 'The new version is Awesome!' );
  Item.ImageIndex := 8;

  Item := lvwEmail.Items.Add;
  Item.Caption := 'Debra Parker';
  Item.SubItems.Add( 'RC is very cool. Keep up the good work.' );
  Item.ImageIndex := 8;

  Item := lvwEmail.Items.Add;
  Item.Caption := 'Dave Sawyer';
  Item.SubItems.Add( 'Where can I get the Signature Controls?' );
  Item.ImageIndex := 9;

  Item := lvwEmail.Items.Add;
  Item.Caption := 'Cindy White';
  Item.SubItems.Add( 'Am I eligible for an upgrade?' );
  Item.ImageIndex := 9;
end;


procedure TFmeCommonControls.PopulateTreeView;
var
  RootNode, ParentNode, Node: TTreeNode;
begin
  RootNode := tvwFolders.Items.Add( nil, 'Account' );
  RootNode.ImageIndex := 0;

  Node := tvwFolders.Items.AddChild( RootNode, 'Calendar' );
  Node.ImageIndex := 1;
  Node.SelectedIndex := Node.ImageIndex;

  ParentNode := tvwFolders.Items.AddChild( RootNode, 'Contacts' );
  ParentNode.ImageIndex := 2;
  ParentNode.SelectedIndex := ParentNode.ImageIndex;

  Node := tvwFolders.Items.AddChild( ParentNode, 'Business' );
  Node.ImageIndex := 2;
  Node.SelectedIndex := Node.ImageIndex;

  Node := tvwFolders.Items.AddChild( ParentNode, 'Personal' );
  Node.ImageIndex := 2;
  Node.SelectedIndex := Node.ImageIndex;

  Node := tvwFolders.Items.AddChild( RootNode, 'Deleted Items' );
  Node.ImageIndex := 3;
  Node.SelectedIndex := Node.ImageIndex;

  Node := tvwFolders.Items.AddChild( RootNode, 'Inbox' );
  Node.ImageIndex := 4;
  Node.SelectedIndex := Node.ImageIndex;

  ParentNode := tvwFolders.Items.AddChild( RootNode, 'Mail' );
  ParentNode.ImageIndex := 5;
  ParentNode.SelectedIndex := ParentNode.ImageIndex;

  Node := tvwFolders.Items.AddChild( ParentNode, 'CodeSite' );
  Node.ImageIndex := 5;
  Node.SelectedIndex := Node.ImageIndex;

  Node := tvwFolders.Items.AddChild( ParentNode, 'Signature Controls' );
  Node.ImageIndex := 5;
  Node.SelectedIndex := Node.ImageIndex;

  Node := tvwFolders.Items.AddChild( ParentNode, 'Conferences' );
  Node.ImageIndex := 5;
  Node.SelectedIndex := Node.ImageIndex;

  Node := tvwFolders.Items.AddChild( RootNode, 'Notes' );
  Node.ImageIndex := 6;
  Node.SelectedIndex := Node.ImageIndex;

  Node := tvwFolders.Items.AddChild( RootNode, 'Tasks' );
  Node.ImageIndex := 7;
  Node.SelectedIndex := Node.ImageIndex;
end;


procedure TFmeCommonControls.UpdateVisualStyle( VS: TRzVisualStyle;
                                                GCS: TRzGradientColorStyle );
begin
  pnlHeader.VisualStyle := VS;
  pnlHeader.GradientColorStyle := GCS;
end;


procedure TFmeCommonControls.lvwEmailChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  lblFrom.Caption := Item.Caption;
  if Item.SubItems.Count > 0 then
  begin
    lblSubject.Caption := Item.SubItems[ 0 ];

    edtMessage.Clear;
    with edtMessage.Lines do
    begin
      if lblFrom.Caption = 'Jennifer Davis' then
      begin
        Add( 'Hi,' );
        Add( 'Congratulatiosn on the new version. I can''t believe all of the new features!' );
        Add( '' );
        Add( 'Jen' );
      end
      else if lblFrom.Caption = 'Arthur Jones' then
      begin
        Add( 'Hi All,' );
        Add( 'Absolutely Awesome! RC4 is an amazing upgrade to an already great product.' );
        Add( '' );
        Add( 'Art Jones' );
      end
      else if lblFrom.Caption = 'Debra Parker' then
      begin
        Add( 'Hello,' );
        Add( 'I just got the new version. The Custom Framing options are very cool! Keep up the great work.' );
        Add( '' );
        Add( 'Debra' );
      end
      else if lblFrom.Caption = 'Dave Sawyer' then
      begin
        Add( 'Hi,' );
        Add( 'The subject says it all.  Where can I purchase the next generation of Konopka Signature VCL Controls?' );
        Add( '' );
        Add( 'Dave' );
      end
      else if lblFrom.Caption = 'Cindy White' then
      begin
        Add( 'I just heard about the new version.  I purchased RC 3.0.7 a while back and I want to know if I''m eligible for an upgrade?' );
        Add( '' );
        Add( 'Cindy' );
      end
      else
        EdtMessage.Clear;
    end;
  end;
end;

procedure TFmeCommonControls.ChkCascadeChecksClick(Sender: TObject);
begin
  tvwFolders.CascadeChecks := ChkCascadeChecks.Checked;
end;

procedure TFmeCommonControls.ChkAlphaSortAllClick(Sender: TObject);
begin
  lvwEmail.AlphaSortAll := ChkAlphaSortAll.Checked;
end;

procedure TFmeCommonControls.ChkFillLastColClick(Sender: TObject);
begin
  lvwEmail.FillLastColumn := ChkFillLastCol.Checked;
end;

end.
