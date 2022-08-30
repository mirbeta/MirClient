{*********************************************************************}
{ Advanced lookup editor component : TAdvLUEdit                       }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by                                                          }
{  TMS Software                                                       }
{  copyright © 2000 - 2012                                            }
{  Email : info@tmssoftware.com                                       }
{  Web : http://www.tmssoftware.com                                   }
{                                                                     }
{ The source code is given as is. The author is not responsible       }
{ for any possible damage done due to the use of this code.           }
{ The component can be freely used in any application. The source     }
{ code remains property of the author and may not be distributed      }
{ freely as such.                                                     }
{*********************************************************************}

unit advlued;

{$I TMSDEFS.INC}

interface

uses
  Windows, Registry, Stdctrls, Classes, Messages, Controls, SysUtils,
  IniFiles, AdvEdit;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.4.0.0 : Improved : inherit latest TAdvEdit improvements
  // v1.4.1.0 : New : ShowFieldName added in TDBAdvLUEdit

type
  TAutoCompleteEvent = procedure(Sender: TObject;const UsrStr:string; var AutoAdd:string;idx:integer) of object;
  TEnterAcceptEvent = procedure(Sender: TObject;const Str:string;idx:integer;var accept:boolean) of object;

  TAddToHistoryEvent = procedure(Sender: TObject; const Str:string) of object;

  TLUPersist = class(TPersistent)
     private
      FEnable:boolean;
      FLocation : TPersistenceLocation;
      FKey : string;
      FSection : string;
      FCount : integer;
      FMaxCount:boolean;
     published
      property Enable:boolean read fEnable write fEnable;
      property Location:TPersistenceLocation read fLocation write fLocation;
      property Key:string read fKey write fKey;
      property Section:string read fSection write fSection;
      property Count:integer read fCount write fCount;
      property MaxCount:boolean read fMaxCount write fMaxCount;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvLUEdit = class(TAdvEdit)
  private
    workmode:boolean;
    FLookupItems:TStringList;
    FAutoComplete:TAutoCompleteEvent;
    FOnAddToHistory:TAddToHistoryEvent;
    FAutoHistory:boolean;
    FAutoSynchronize:boolean;
    FEnterAccept:TEnterAcceptEvent;
    FFileLookup:boolean;
    FLUPersist:TLUPersist;
    FMatchCase:boolean;
    FChanged:boolean;
    procedure SetLookupItems(value:tstringlist);
    procedure SetLUPersist(value:TLUPersist);
    procedure WMDestroy(var Msg:TMessage); message wm_Destroy;
  protected
    function GetVersionNr: Integer; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
    procedure LookupText;
  public
    procedure Change; override;
    constructor Create(aOwner:tComponent); override;
    destructor Destroy; override;
    procedure SavePersist; override;
    procedure LoadPersist; override;
    procedure Loaded; override;
  published
    property LookupItems: TStringList read fLookupItems write SetLookupItems;
    property AutoComplete: TAutoCompleteEvent read FAutoComplete write FAutoComplete;
    property OnAddToHistory: TAddToHistoryEvent read FOnAddToHistory write FOnAddToHistory;
    property Accept: TEnterAcceptEvent read FEnterAccept write FEnterAccept;
    property AutoHistory: boolean read FAutoHistory write FAutoHistory;
    property AutoSynchronize: boolean read fAutoSynchronize write fAutoSynchronize;
    property FileLookup: boolean read fFileLookup write fFileLookup;
    property LookupPersist: TLUPersist read fLUPersist write SetLUPersist;
    property MatchCase: boolean read FMatchCase write FMatchCase;
  end;

implementation

function upstr(s:string;docase:boolean):string;
begin
  if docase then
    Result := s
  else
    Result := AnsiUpperCase(s);
end;

function ncpos(su,s:string):integer;
begin
  su := upstr(su,false);
  s := upstr(s,false);
  Result := pos(su,s);
end;

procedure TAdvLUEdit.SetLookupItems(value:tstringlist);
begin
  if Assigned(value) then
    FLookupItems.Assign(Value)
end;

procedure TAdvLUEdit.DoExit;
var
  AllowExit: boolean;
  i: integer;
  lu: TAdvLUEdit;
begin
  AllowExit := true;

  if Assigned(FEnterAccept) then
  begin
    FEnterAccept(Self,Text,FLookupItems.IndexOf(Text), AllowExit);
  end;

  if AllowExit and fautohistory and (text<>'') then
  begin
    if (FLookupitems.indexof(text)=-1) then
     begin
       FLookupitems.Add(text);
       if Assigned(FOnAddToHistory) then
         FOnAddToHistory(self,text);

       if FAutoSynchronize then
       for I := 0 to Owner.ComponentCount - 1 do
         begin
           if (Owner.Components[I] is TAdvLUEdit) and (Owner.Components[I]<>self) then
            begin
             lu := Owner.Components[I] as TAdvLUEdit;
             if (lu.AutoHistory) and (lu.LookupPersist.key = self.LookupPersist.Key) and (lu.AutoSynchronize) and
                (lu.LookupPersist.Section = self.LookupPersist.Section) then
                 lu.LookupItems.Assign(self.LookupItems);
             end;
         end;
     end;
   end;

  if AllowExit then
    inherited DoExit
  else
    SetFocus;
end;

procedure TAdvLUEdit.LookupText;
var
  c:string;
  i:integer;
  UsrStr,AutoAdd:string;
  searchrec:TSearchRec;

begin
// inherited Change;

  if csDesigning in ComponentState then
    Exit;

  if not workmode then
    Exit;

  c := UpStr(Text,fMatchCase);
  c := Copy(c,1,selstart);

  if (fLookupItems.count>0) then
  for i:=0 to fLookupItems.count-1 do
    begin
     if pos(c,upstr(fLookupItems.Strings[i],fMatchCase))=1 then
      begin
       UsrStr:=copy(text,1,length(c));
       AutoAdd:=copy(fLookupItems.Strings[i],length(c)+1,255);
       if assigned(FAutoComplete) then
         begin
          FAutoComplete(self,UsrStr,AutoAdd,i);
         end;

       Text:=UsrStr+AutoAdd;
       Modified := True;
       SendMessage(Handle,EM_SETSEL,length(c)+length(Prefix),length(text)+length(Prefix));
       Exit;
      end;
   end;

  if (fLookupItems.count=0) and (length(self.text)>0) and (fFileLookup) then
  begin
    if FindFirst(self.text+'*' ,faAnyfile,searchrec)=0 then
    begin
      c:=text;
      usrstr:='';
      while (pos(':',c)>0) do
      begin
        usrstr:=usrstr+copy(c,1,pos(':',c));
        delete(c,1,pos(':',c));
      end;
      while (pos('\',c)>0) do
      begin
        usrstr:=usrstr+copy(c,1,pos('\',c));
        delete(c,1,pos('\',c));
      end;

      if ((ncpos(c,searchrec.name)=1) or (c=''))
        and (ncpos('.',searchrec.name)<>1) then
      begin
        c:=text;
        text:=usrstr+searchrec.name;
        Modified := True;
        SendMessage(Handle,EM_SETSEL,length(c),length(text));
      end;
    end;

    FindClose(searchrec);
  end;
end;

procedure TAdvLUEdit.Change;
begin
  inherited;
  FChanged := True;
end;

procedure TAdvLUEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
  vk_back,vk_delete:workmode:=false;
  vk_return:
    begin
      if (fLookupItems.IndexOf(Text)<>-1) then
      begin
        text := FLookupItems.Strings[fLookupItems.IndexOf(Text)];
        Change;
      end;
    end;
  else
    workmode := true;
  end;

  inherited KeyDown(key,shift);
end;


constructor TAdvLUEdit.Create(aOwner:tComponent);
begin
  inherited Create(aOwner);
  fLookupItems := TStringList.Create;
  fLUPersist := TLUPersist.Create;
  workmode:=true;
end;


destructor TAdvLUEdit.Destroy;
begin
  FLookupItems.Free;
  FLookupItems := nil;
  FLUPersist.Free;
  FLUPersist := nil;
  inherited Destroy;
end;

procedure TAdvLUEdit.SetLUPersist(value: TLUPersist);
begin
  FLUPersist.Assign(value);
end;

procedure TAdvLUEdit.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    LoadPersist;
end;

procedure TAdvLUEdit.WMDestroy(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) then
    SavePersist;
  DefaultHandler(msg);
end;

procedure TAdvLUEdit.LoadPersist;
var
  Inifile: TCustomInifile;
  i: Integer;
  s: string;
begin
  inherited LoadPersist;
  if FLUPersist.Enable then
  begin
    if (FLUPersist.Location = plInifile) then
      Inifile := TInifile.Create(self.FLUPersist.Key)
    else
      Inifile := TRegistryInifile.Create(self.FLUPersist.Key);

    LookupItems.Clear;
    i := 1;
    repeat
      s := Inifile.ReadString(FLUPersist.Section,'Item'+IntToStr(i),'');
      Inc(i);
      if s <> '' then self.LookupItems.Add(s);
    until s = '';
    Inifile.Free;
  end;
end;

procedure TAdvLUEdit.SavePersist;
var
  Inifile: TCustomInifile;
  i,j,k: Integer;
begin
  inherited SavePersist;

  if Assigned(FLUPersist) and FLUPersist.Enable and Assigned(LookupItems) then
  begin
    if FLUPersist.Location = plInifile then
      Inifile := TInifile.Create(fLUPersist.Key)
    else
      Inifile := TRegistryInifile.Create(FLUPersist.Key);

    j := 0;
    k := LookupItems.Count;
    if FLUPersist.MaxCount then
      k := FLUPersist.Count;

    if FLUPersist.MaxCount and (LookupItems.Count > FLUPersist.Count) then
      j := LookUpItems.Count - FLuPersist.Count;

    for i := 1 to k do
    begin
      if (i + j <= LookUpItems.Count) then
        Inifile.WriteString(FLUPersist.Section,'Item'+IntToStr(i),LookupItems.Strings[i+j-1])
      else
        Inifile.WriteString(FLUPersist.Section,'Item'+IntToStr(i),'');
    end;
    Inifile.Free;
  end;
end;


procedure TAdvLUEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if FChanged and not (key in [vk_back,vk_delete]) then LookupText;
  FChanged := False;
  inherited;
end;

function TAdvLUEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

end.
