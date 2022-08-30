{=======================================================================================================================
  SendMessageFrame Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2002 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

unit SendMessageFrame;

interface

uses
  Forms,
  ImgList,
  Controls,
  RzShellDialogs,
  RzSndMsg,
  Mask,
  RzEdit,
  StdCtrls,
  RzLstBox,
  RzPanel,
  RzButton,
  Classes,
  RzCommon,
  RzLabel,
  ExtCtrls;

type
  TFmeSendMessages = class(TFrame)
    Msg: TRzSendMessage;
    RzToolbar1: TRzToolbar;
    pnlMessage: TRzPanel;
    EdtMessage: TRzMemo;
    GrpAttach: TRzPanel;
    LstFiles: TRzEditListBox;
    pnlRecipients: TRzPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EdtTO: TRzMemo;
    EdtCC: TRzMemo;
    EdtSubject: TRzEdit;
    DlgOpen: TRzOpenDialog;
    BtnSend: TRzToolButton;
    BtnAttach: TRzToolButton;
    ImageList1: TImageList;
    pnlHeader: TRzPanel;
    RzLabel1: TRzLabel;
    procedure BtnSendClick(Sender: TObject);
    procedure BtnAttachClick(Sender: TObject);
    procedure BtnRemoveClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Init;
    procedure UpdateVisualStyle( VS: TRzVisualStyle; GCS: TRzGradientColorStyle );
  end;


implementation

{$R *.dfm}


procedure TFmeSendMessages.Init;
begin
//  ParentBackground := False;
end;


procedure TFmeSendMessages.UpdateVisualStyle( VS: TRzVisualStyle;
                                              GCS: TRzGradientColorStyle );
begin
  pnlHeader.VisualStyle := VS;
  pnlHeader.GradientColorStyle := GCS;
  RzToolbar1.VisualStyle := VS;
  RzToolbar1.GradientColorStyle := GCS;
end;


procedure TFmeSendMessages.BtnSendClick(Sender: TObject);
begin
  Msg.Attachments := LstFiles.Items;
  Msg.ToRecipients := EdtTO.Lines;
  Msg.CcRecipients := EdtCC.Lines;
  Msg.Send;
  { Clear all fields }
  EdtTo.Clear;
  EdtCC.Clear;
  EdtSubject.Clear;
  EdtMessage.Clear;
  LstFiles.Clear;
end;


procedure TFmeSendMessages.BtnAttachClick(Sender: TObject);
begin
  if DlgOpen.Execute then
    LstFiles.Items.Add( DlgOpen.FileName );
end;


procedure TFmeSendMessages.BtnRemoveClick(Sender: TObject);
begin
  if LstFiles.ItemIndex <> -1 then
    LstFiles.Items.Delete( LstFiles.ItemIndex );
end;


end.
