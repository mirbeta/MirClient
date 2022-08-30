{********************************************************************}
{ TLISTLINK component                                                }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{  TMS Software                                                      }
{  copyright © 1997-2011                                             }
{  Email : info@tmssoftware.com                                      }
{  Web : http://www.tmssoftware.com                                  }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such nor sold in any form.                               }
{********************************************************************}

unit listlink;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, stdctrls, comctrls;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

type

  TClickEvent = procedure (Sender:TObject) of object;

  TDragDropMode = (ddCopy,ddMove);

  TLinkMode = (lmListBox, lmListView);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TListLink = class(TComponent)
  private
   FLinkMode: TLinkMode;

   FMoveLR:TButton;
   FMoveRL:TButton;
   FMoveSelLR:TButton;
   FMoveSelRL:TButton;

   FCopyLR:TButton;
   FCopyRL:TButton;
   FCopySelLR:TButton;
   FCopySelRL:TButton;

   FLList:TListBox;
   FRList:TListBox;
   FLListView:TListView;
   FRListView:TListView;

   FMoveLRClick:tclickevent;
   FMoveRLClick:tclickevent;
   FMoveSelLRClick:tclickevent;
   FMoveSelRLClick:tclickevent;
   FCopyLRClick:tclickevent;
   FCopyRLClick:tclickevent;
   FCopySelLRClick:tclickevent;
   FCopySelRLClick:tclickevent;

   FOnModified: TNotifyEvent;

   MoveLRAssigned:boolean;
   MoveRLAssigned:boolean;
   MoveSelLRAssigned:boolean;
   MoveSelRLAssigned:boolean;
   CopyLRAssigned:boolean;
   CopyRLAssigned:boolean;
   CopySelLRAssigned:boolean;
   CopySelRLAssigned:boolean;

   FDragDropMode:TDragDropMode;

   procedure SetMoveLR(abutton:TButton);
   procedure SetMoveRL(abutton:TButton);
   procedure SetMoveSelLR(abutton:TButton);
   procedure SetMoveSelRL(abutton:TButton);
   procedure SetCopyLR(abutton:TButton);
   procedure SetCopyRL(abutton:TButton);
   procedure SetCopySelLR(abutton:TButton);
   procedure SetCopySelRL(abutton:TButton);

   procedure SetLList(alist:TListBox);
   procedure SetRList(alist:TListBox);
   procedure MoveLRClick(Sender:TObject);
   procedure MoveRLClick(Sender:TObject);
   procedure MoveSelLRClick(Sender:TObject);
   procedure MoveSelRLClick(Sender:TObject);
   procedure CopyLRClick(Sender:TObject);
   procedure CopyRLClick(Sender:TObject);
   procedure CopySelLRClick(Sender:TObject);
   procedure CopySelRLClick(Sender:TObject);

   procedure DragOverLList(Sender, Source: TObject; X, Y: Integer;
                           State: TDragState; var Accept: Boolean);
   procedure DragOverRList(Sender, Source: TObject; X, Y: Integer;
                           State: TDragState; var Accept: Boolean);
   procedure DragDropLList(Sender, Source: TObject;X, Y: Integer);
   procedure DragDropRList(Sender, Source: TObject;X, Y: Integer);

   procedure DragOverLListView(Sender, Source: TObject; X, Y: Integer;
                               State: TDragState; var Accept: Boolean);
   procedure DragOverRListView(Sender, Source: TObject; X, Y: Integer;
                               State: TDragState; var Accept: Boolean);
   procedure DragDropLListView(Sender, Source: TObject;X, Y: Integer);
   procedure DragDropRListView(Sender, Source: TObject;X, Y: Integer);

   procedure MoveListData(sourcelist,targetlist:TListBox);
   procedure MoveSelListData(sourcelist,targetlist:TListBox);
   procedure MoveSelListViewData(sourcelist,targetlist:TListView);
   procedure CopyListData(sourcelist,targetlist:TListBox;idx:integer);
   procedure CopySelListData(sourcelist,targetlist:TListBox);
   procedure CopySelListViewData(sourcelist,targetlist:TListView);
   procedure CopyEntireList(sourcelist,targetlist:TListBox);
   procedure MoveEntireList(sourcelist,targetlist:TListBox);
   procedure CopyEntireListView(sourcelist,targetlist:TListView);
   procedure MoveEntireListView(sourcelist,targetlist:TListView);
   procedure SetLListView(const Value: TListView);
   procedure SetRListView(const Value: TListView);
   function GetVersion: string;
   procedure SetVersion(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    function GetVersionNr: Integer; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property MoveLR:TButton read FMoveLR write SetMoveLR;
    property MoveRL:TButton read FMoveRL write SetMoveRL;
    property MoveSelLR:TButton read FMoveSelLR write SetMoveSelLR;
    property MoveSelRL:TButton read FMoveSelRL write SetMoveSelRL;
    property CopyLR:TButton read FCopyLR write SetCopyLR;
    property CopyRL:TButton read FCopyRL write SetCopyRL;
    property CopySelLR:TButton read FCopySelLR write SetCopySelLR;
    property CopySelRL:TButton read FCopySelRL write SetCopySelRL;

    property LinkMode:TLinkMode read fLinkMode write fLinkMode;

    property LList:TListBox read FLList write SetLList;
    property RList:TListBox read FRList write SetRList;
    property LListView:TListView read FLListView write SetLListView;
    property RListView:TListView read FRListView write SetRListView;
    property DragDropMode:TDragDropMode read FDragDropMode write FDragDropMode;

    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

constructor TListLink.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 MoveLRAssigned:=false;
 MoveRLAssigned:=false;
 MoveSelLRAssigned:=false;
 MoveSelRLAssigned:=false;
 CopyLRAssigned:=false;
 CopyRLAssigned:=false;
 CopySelLRAssigned:=false;
 CopySelRLAssigned:=false;
end;

destructor TListLink.Destroy;
begin
 inherited;
end;

procedure TListLink.SetLList(alist:TListBox);
begin
 FLList:=alist;
 if aList=nil then exit;
 FLList.OnDragDrop:=DragDropLList;
 FLList.OnDragOver:=DragOverLList;
end;

procedure TListLink.SetRList(alist:TListBox);
begin
 FRList:=alist;
 if aList=nil then exit;
 FRList.OnDragDrop:=DragDropRList;
 FRList.onDragOver:=DragOverRList;
end;

procedure TListLink.SetMoveLR(abutton:TButton);
begin
 MoveLRAssigned:=false;
 FMoveLR:=abutton;
 if abutton=nil then exit;
 if assigned(FMoveLR.OnClick) then
   begin
    FMoveLRClick:=FMoveLR.OnClick;
    MoveLRAssigned:=true;
   end;
 FMoveLR.OnClick:=MoveLRClick;
end;

procedure TListLink.SetMoveRL(abutton:TButton);
begin
 MoveRLAssigned:=false;
 FMoveRL:=abutton;
 if abutton=nil then exit;
 if assigned(FMoveRL.OnClick) then
   begin
    FMoveRLClick:=FMoveRL.OnClick;
    MoveRLAssigned:=true;
   end;
 FMoveRL.OnClick:=MoveRLClick;
end;

procedure TListLink.SetMoveSelLR(abutton:TButton);
begin
 MoveSelLRAssigned:=false;
 FMoveSelLR:=abutton;
 if abutton=nil then exit;
 if assigned(FMoveSelLR.OnClick) then
   begin
    FMoveSelLRClick:=FMoveSelLR.OnClick;
    MoveSelLRAssigned:=true;
   end;
 FMoveSelLR.OnClick:=MoveSelLRClick;
end;

procedure TListLink.SetMoveSelRL(abutton:TButton);
begin
 MoveSelRLAssigned:=false;
 FMoveSelRL:=abutton;
 if abutton=nil then exit;
 if assigned(FMoveSelRL.OnClick) then
   begin
    FMoveSelRLClick:=FMoveSelRL.OnClick;
    MoveSelRLAssigned:=true;
   end;
 FMoveSelRL.OnClick:=MoveSelRLClick;
end;

procedure TListLink.SetCopyLR(abutton:TButton);
begin
 CopyLRAssigned:=false;
 FCopyLR:=abutton;
 if abutton=nil then exit;
 if assigned(FCopyLR.OnClick) then
   begin
    FCopyLRClick:=FCopyLR.OnClick;
    CopyLRAssigned:=true;
   end;
 FCopyLR.OnClick:=CopyLRClick;
end;

procedure TListLink.SetCopyRL(abutton:TButton);
begin
 CopyRLAssigned:=false;
 FCopyRL:=abutton;
 if abutton=nil then exit;
 if assigned(FCopyRL.OnClick) then
   begin
    FCopyRLClick:=FCopyRL.OnClick;
    CopyRLAssigned:=true;
   end;
 FCopyRL.OnClick:=CopyRLClick;
end;

procedure TListLink.SetCopySelLR(abutton:TButton);
begin
 CopySelLRAssigned:=false;
 FCopySelLR:=abutton;
 if abutton=nil then exit;
 if assigned(FCopySelLR.OnClick) then
   begin
    FCopySelLRClick:=FCopySelLR.OnClick;
    CopySelLRAssigned:=true;
   end;
 FCopySelLR.OnClick:=CopySelLRClick;
end;

procedure TListLink.SetCopySelRL(abutton:TButton);
begin
 CopySelRLAssigned:=false;
 FCopySelRL:=abutton;
 if abutton=nil then exit;
 if assigned(FCopySelRL.OnClick) then
   begin
    FCopySelRLClick:=FCopySelRL.OnClick;
    CopySelRLAssigned:=true;
   end;
 FCopySelRL.OnClick:=CopySelRLClick;
end;

procedure TListLink.MoveEntireList(sourcelist,targetlist:TListBox);
begin
 CopyEntireList(sourcelist,targetlist);
 sourceList.items.clear;
end;

procedure TListLink.MoveEntireListView(sourcelist,targetlist:TListView);
begin
 CopyEntireListView(sourcelist,targetlist);
 sourceList.items.clear;
end;


procedure TListLink.CopyEntireList(sourcelist,targetlist:TListBox);
begin
 if not assigned(sourceList) then exit;
 if not assigned(targetList) then exit;
 if sourceList.items.count = 0 then exit;
 targetList.items.Addstrings(sourceList.items);

 if assigned(FOnModified) then FOnModified(Self);
end;

procedure TListLink.CopyEntireListView(sourcelist,targetlist:TListView);
begin
 if not assigned(sourceList) then exit;
 if not assigned(targetList) then exit;
 if sourceList.items.count = 0 then exit;
 targetlist.items.Assign(sourcelist.items);
 
 if assigned(FOnModified) then FOnModified(Self);
end;


procedure TListLink.MoveListData(sourcelist,targetlist:TListBox);
var
 data:longint;
 idx:integer;
begin
 data:=sendmessage(sourcelist.handle,lb_getitemdata,sourcelist.itemindex,0);

 idx:=targetlist.items.add(sourcelist.items[sourcelist.itemindex]);
 sendmessage(targetlist.handle,lb_setitemdata,idx,data);
 targetlist.itemindex:=idx;
 if targetlist.multiselect then
    targetlist.selected[idx]:=true;

 idx:=sourcelist.itemindex;
 sourcelist.items.delete(sourcelist.itemindex);
 if (idx>=sourcelist.items.count) then idx:=sourcelist.items.count-1;
 sourcelist.itemindex:=idx;
 if sourcelist.multiselect then
    sourcelist.selected[idx]:=true;

 if assigned(FOnModified) then FOnModified(Self);
end;

procedure TListLink.CopyListData(sourcelist,targetlist:TListBox;idx:integer);
var
 data:longint;
 pos:integer;
begin
 data:=sendmessage(sourcelist.handle,lb_getitemdata,idx,0);
 pos:=targetlist.items.add(sourcelist.items[idx]);
 sendmessage(targetlist.handle,lb_setitemdata,pos,data);
 targetlist.itemindex:=pos;
 if targetlist.multiselect then targetlist.selected[pos]:=true;

 if assigned(FOnModified) then FOnModified(Self);
end;

procedure TListLink.CopySelListData(sourcelist,targetlist:TListBox);
var
 i:integer;
begin
 if not assigned(SourceList) then exit;
 if not assigned(TargetList) then exit;
 if SourceList.Items.Count<=0 then exit;

 for i:=0 to sourcelist.items.count-1 do
  begin
   if sourcelist.selected[i] then
     begin
       CopyListData(sourcelist,targetlist,i);
     end;
  end;
end;

procedure TListLink.CopySelListViewData(sourcelist,targetlist:TListView);
var
 i:integer;
begin
 if not assigned(SourceList) then exit;
 if not assigned(TargetList) then exit;
 if SourceList.Items.Count<=0 then exit;

 for i:=0 to sourcelist.items.count-1 do
  begin
   if sourcelist.Items[i].Selected then
     begin
       targetlist.Items.Add.Assign(sourcelist.Items[i]);
     end;
  end;

 if assigned(FOnModified) then FOnModified(Self);
end;


procedure TListLink.MoveSelListData(sourcelist,targetlist:TListBox);
var
 i,j:integer;
begin
 if not assigned(SourceList) then exit;
 if not assigned(TargetList) then exit;
 if SourceList.Items.Count<=0 then exit;

 for i:=0 to sourcelist.items.count-1 do
  begin
   if sourcelist.selected[i] then
     begin
       CopyListData(sourcelist,targetlist,i);
     end;
  end;

  i:=0;
  j:=0;
  while (i<=sourcelist.items.count-1) and (sourcelist.items.count>=0) do
   begin
    if sourcelist.selected[i] then
      begin
       sourcelist.items.delete(i);
       j:=i;
      end
    else
      inc(i);
   end;

  if (j>=sourcelist.items.count) then dec(j);
  if (j>=0) then sourcelist.itemindex:=j;

end;

procedure TListLink.MoveSelListViewData(sourcelist,targetlist:TListView);
var
 i,j:integer;
begin
 if not assigned(SourceList) then exit;
 if not assigned(TargetList) then exit;
 if SourceList.Items.Count<=0 then exit;

 for i:=0 to sourcelist.items.count-1 do
  begin
   if sourcelist.Items[i].Selected then
     begin
       targetlist.Items.Add.Assign(sourcelist.Items[i]);
     end;
  end;

  i:=0;
  j:=0;
  while (i<=sourcelist.items.count-1) and (sourcelist.items.count>=0) do
   begin
    if sourcelist.items[i].selected then
      begin
       sourcelist.items[i].Free;
       j:=i;
      end
    else
      inc(i);
   end;

  if (j>=sourcelist.items.count) then dec(j);
  if (j>=0) then
    begin
     sourcelist.Selected:=sourcelist.items[j];
     sourcelist.items[j].Selected:=true;
    end;

 if assigned(FOnModified) then FOnModified(Self);
end;


procedure TListLink.MoveSelRLClick(Sender:TObject);
begin
 if MoveSelRLassigned then
    begin
     try
      FMoveSelRLClick(Sender);
     except
     end;
    end;

 if fLinkMode=lmListBox then
  MoveSelListData(FRList,FLList)
 else
  MoveSelListViewData(FRListView,FLListView);
end;

procedure TListLink.MoveRLClick(sender:tobject);
begin
 if MoveRLassigned then
    begin
     try
      FMoveRLClick(Sender);
     except
     end;
    end;
 if fLinkMode=lmListBox then
  MoveEntireList(FRList,FLList)
 else
  MoveEntireListView(FRListView,FLListView);
end;

procedure TListLink.MoveLRClick(sender:tobject);
begin
  if MoveLRassigned then
    begin
     try
      FMoveLRClick(Sender);
     except
     end;
    end;
  if fLinkMode=lmListBox then
   MoveEntireList(FLList,FRList)
  else
   MoveEntireListView(FLListView,FRListView);
end;

procedure TListLink.MoveSelLRClick(Sender:TObject);
begin
 if MoveSelLRassigned then
    begin
     try
      FMoveSelLRClick(Sender);
     except
     end;
    end;
 if fLinkMode=lmListBox then
  MoveSelListData(FLList,FRList)
 else
  MoveSelListViewData(FLListView,FRListView);
end;

procedure TListLink.CopySelRLClick(Sender:TObject);
begin
 if CopySelRLassigned then
    begin
     try
      FCopySelRLClick(Sender);
     except
     end;
    end;

 if fLinkMode=lmListBox then
  CopySelListData(FRList,FLList)
 else
  CopySelListViewData(FRListView,FLListView)
end;

procedure TListLink.CopyRLClick(sender:tobject);
begin
 if CopyRLassigned then
    begin
     try
      FCopyRLClick(Sender);
     except
     end;
    end;
 if fLinkMode=lmListBox then
  CopyEntireList(FRList,FLList)
 else
  CopyEntireListView(FRListView,FLListView);
end;

procedure TListLink.CopyLRClick(sender:tobject);
begin
 if CopyLRassigned then
    begin
     try
      FCopyLRClick(Sender);
     except
     end;
    end;

 if fLinkMode=lmListBox then
   CopyEntireList(FLList,FRList)
 else
   CopyEntireListView(FLListView,FRListView);
end;

procedure TListLink.CopySelLRClick(Sender:TObject);
begin
 if CopySelLRassigned then
    begin
     try
      FCopySelLRClick(Sender);
     except
     end;
    end;

 if fLinkMode=lmListBox then
  CopySelListData(FLList,FRList)
 else
  CopySelListViewData(FLListView,FRListView);
end;

procedure TListLink.DragOverLList(Sender, Source: TObject; X, Y: Integer;
                           State: TDragState; var Accept: Boolean);
begin
  accept:=(source=rlist);
end;

procedure TListLink.DragOverRList(Sender, Source: TObject; X, Y: Integer;
                           State: TDragState; var Accept: Boolean);
begin
  accept:=(source=llist);
end;

procedure TListLink.DragDropLList(Sender, Source: TObject;X, Y: Integer);
begin
 if (FRList.itemindex<>-1) and (source=FRList) then
  begin
   if (FDragDropMode=ddMove) then
    begin
     if FRList.multiselect then
       MoveSelListData(FRList,FLList)
     else
       MoveListData(FRList,FLList);
    end
   else
    begin
     if FRList.multiselect then
       CopySelListData(FRList,FLList)
     else
       CopyListData(FRList,FLList,FRList.itemindex);
    end;
  end;
end;

procedure TListLink.DragDropRList(Sender, Source: TObject;X, Y: Integer);
begin
 if (FLList.itemindex<>-1) and (source=FLList) then
   begin
    if (FDragDropMode=ddMove) then
     begin
      if FLList.multiselect then
        MoveSelListData(FLList,FRList)
      else
        MoveListData(FLList,FRList);
     end
    else
     begin
      if FLList.multiselect then
        CopySelListData(FLList,FRList)
      else
        CopyListData(FLList,FRList,FLList.Itemindex);
     end;

   end;
end;

procedure TListLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent, Operation);

 if (Operation = opRemove) and (AComponent = fLList) then fLList := nil;
 if (Operation = opRemove) and (AComponent = fRList) then fRList := nil;
 if (Operation = opRemove) and (AComponent = fLListView) then fLListView := nil;
 if (Operation = opRemove) and (AComponent = fRListView) then fRListView := nil;

 if (Operation = opRemove) and (AComponent = fMoveLR) then fMoveLR := nil;
 if (Operation = opRemove) and (AComponent = fMoveRL) then fMoveRL := nil;
 if (Operation = opRemove) and (AComponent = fMoveSelLR) then fMoveSelLR := nil;
 if (Operation = opRemove) and (AComponent = fMoveSelRL) then fMoveSelRL := nil;

 if (Operation = opRemove) and (AComponent = fCopyLR) then fCopyLR := nil;
 if (Operation = opRemove) and (AComponent = fCopyRL) then fCopyRL := nil;
 if (Operation = opRemove) and (AComponent = fCopySelLR) then fCopySelLR := nil;
 if (Operation = opRemove) and (AComponent = fCopySelRL) then fCopySelRL := nil;
end;


procedure TListLink.SetLListView(const Value: TListView);
begin
 FLListView := Value;
 FLListView.OnDragDrop:=DragDropLListView;
 FLListView.OnDragOver:=DragOverLListView;
end;

procedure TListLink.SetRListView(const Value: TListView);
begin
 FRListView := Value;
 FRListView.OnDragDrop:=DragDropRListView;
 FRListView.OnDragOver:=DragOverRListView;
end;

procedure TListLink.DragDropLListView(Sender, Source: TObject; X,
  Y: Integer);
begin
 if assigned(FRListView.Selected) and (source=FRListView) then
  begin
   if (FDragDropMode=ddMove) then
    begin
      MoveSelListViewData(FRListView,FLListView);
    end
   else
    begin
       CopySelListViewData(FRListView,FLListView);
    end;
  end;

end;

procedure TListLink.DragDropRListView(Sender, Source: TObject; X,
  Y: Integer);
begin
 if assigned(FLListView.Selected) and (source=FLListView) then
  begin
   if (FDragDropMode=ddMove) then
    begin
      MoveSelListViewData(FLListView,FRListView);
    end
   else
    begin
       CopySelListViewData(FLListView,FRListView);
    end;
  end;
end;

procedure TListLink.DragOverLListView(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  accept:=(source=rlistview);
end;

procedure TListLink.DragOverRListView(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  accept:=(source=llistview);
end;

function TListLink.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TListLink.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TListLink.SetVersion(const Value: string);
begin

end;

end.
