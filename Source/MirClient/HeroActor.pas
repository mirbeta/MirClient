unit HeroActor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grobal2, PXL.Canvas, PXL.Textures, cliUtil, magiceff, WIL, ClFunc, GList, Actor, MShare;

const
  overdisc                  = 22;

procedure Init_Queue();
procedure Init_Queue2();
function IsBackToSafeZone(var ret: Integer): Boolean;
function GetDis(X1, Y1, X2, Y2: Integer): Integer;
function IsProperTarget(Actor: TActor): Boolean;
function SearchTarget(): TActor;
function GetDropItemsDis(): Integer;
function GetAutoPalyStation: Integer;
function AutoUseMagic(Key: Byte; target: TActor; nx: Integer = -1; ny: Integer = -1): Boolean;
function HeroAttackTagget(target: TActor): Boolean;
procedure enter_queue(Node: PTree; F: Integer);
function get_from_queue: PTree;
procedure FreeTree();
function judge(X, Y, end_x, end_y: Integer): Integer;
function Trytile(X, Y, end_x, end_y: Integer; Father: PTree; dir: Byte): Boolean;
procedure AP_findpath(Startx, Starty, end_x, end_y: Integer);
function RandomRange(const AFrom, ATo: Integer): Integer;

function TargetCount(target: TActor): Integer;
function TargetCount2(target: TActor): Integer;
function TargetCount3(target: TActor): Integer;
function TargetHumCount(target: TActor): Integer;

var
  g_hinttick1, g_hinttick2  : LongWord;

implementation

uses
  ClMain, SoundUtil, clEvent, HUtil32;

// ������ڵ������, ������Ŀ�ĵع��۾����������

function CanNextSpell(): Boolean;
begin
Result := False;
        if g_boSpeedRate then begin
          if GetTickCount - g_dwLatestSpellTick > (g_dwSpellTime + g_dwMagicDelayTime - g_MagSpeedRate * 20) then begin
            Result := True;
          end;
        end else begin
          if GetTickCount - g_dwLatestSpellTick > (g_dwSpellTime + g_dwMagicDelayTime) then begin
            Result := True;
          end;
        end;
end;

procedure enter_queue(Node: PTree; F: Integer);
var
  P, Father, q              : PLink;
begin
  P := g_APQueue;
  Father := P;
  while (F > P.F) do begin
    Father := P;
    P := P.Next;
    if P = nil then
      Break;
  end;
  New(q);
  q.F := F;
  q.Node := Node;
  q.Next := P;
  Father.Next := q;
end;

// ����Ŀ�ĵع�������ķ���������

function get_from_queue: PTree;
var
  bestchoice                : PTree;
  Next                      : PLink;
begin
  bestchoice := g_APQueue.Next.Node;
  Next := g_APQueue.Next.Next;
  Dispose(g_APQueue.Next);
  g_APQueue.Next := Next;
  Result := bestchoice;
end;

// �ͷ�����������нڵ�

procedure FreeTree();
var
  P                         : PLink;
begin
  while (g_APQueue <> nil) do begin
    P := g_APQueue;
    if P.Node <> nil then
      Dispose(P.Node);
    P.Node := nil;    
    g_APQueue := g_APQueue.Next;
    Dispose(P);
  end;
end;

// ���ۺ���,���� x,y ��Ŀ�ĵصľ���,����ֵ���뱣֤��ʵ��ֵС

function judge(X, Y, end_x, end_y: Integer): Integer;
begin
  Result := abs(end_x - X) + abs(end_y - Y);
end;

function Trytile(X, Y, end_x, end_y: Integer; Father: PTree; dir: Byte): Boolean;
var
  P                         : PTree;
  H                         : Integer;

  function has(X, Y, H: Integer): Boolean;
  var
    cx, cy                  : Integer;
  begin
    Result := True;
    cx := X - Map.m_nBlockLeft;
    cy := Y - Map.m_nBlockTop;
    if (cx > MAXX * 3) or (cy > MAXY * 3) then Exit;
    if (cx < 0) or (cy < 0) then Exit;
    if H < g_APPass^[cx, cy] then
      Result := False;
  end;

begin
  Result := False;
  if not Map.CanMove(X, Y) then
    Exit;
  P := Father;
  while (P <> nil) do begin
    if (X = P.X) and (Y = P.Y) then
      Exit;                             //��� (x,y) ��������,ʧ��
    P := P.Father;
  end;
  H := Father.H + 1;
  if has(X, Y, H) then
    Exit;                               // ��������и��õķ����ƶ��� (x,y) ʧ��
  g_APPass^[X - Map.m_nBlockLeft, Y - Map.m_nBlockTop] := H; // ��¼��ε� (x,y) �ľ���Ϊ��ʷ��Ѿ���

  New(P);
  P.Father := Father;
  P.H := Father.H + 1;
  P.X := X;
  P.Y := Y;
  P.dir := dir;
  enter_queue(P, P.H + judge(X, Y, end_x, end_y));
  Result := True;
end;

// ·��Ѱ��������

procedure AP_findpath(Startx, Starty, end_x, end_y: Integer);
var
  Root, P                   : PTree;
  i, X, Y, dir              : Integer;
  Temp                      : PFindNOde;
begin
  if not Map.CanMove(end_x, end_y) then
    Exit;

  FillChar(g_APPass^, SizeOf(TAPPass), $FF);

  Init_Queue();

  New(Root);
  Root.X := Startx;
  Root.Y := Starty;
  Root.H := 0;
  Root.Father := nil;
  enter_queue(Root, judge(Startx, Starty, end_x, end_y));

  while (True) do begin
    Root := get_from_queue();           //����һ������
    if Root = nil then
      Break;
    X := Root.X;
    Y := Root.Y;
    if (X = end_x) and (Y = end_y) then
      Break;
    Trytile(X, Y - 1, end_x, end_y, Root, 0); //���������ƶ�
    Trytile(X + 1, Y - 1, end_x, end_y, Root, 1); //�����������ƶ�
    Trytile(X + 1, Y, end_x, end_y, Root, 2); //���������ƶ�
    Trytile(X + 1, Y + 1, end_x, end_y, Root, 3); //�����������ƶ�
    Trytile(X, Y + 1, end_x, end_y, Root, 4); //���������ƶ�
    Trytile(X - 1, Y + 1, end_x, end_y, Root, 5); //�����������ƶ�
    Trytile(X - 1, Y, end_x, end_y, Root, 6); //���������ƶ�
    Trytile(X - 1, Y - 1, end_x, end_y, Root, 7); //�����������ƶ�
  end;

  for i := g_APPathList.count - 1 downto 0 do begin
    Dispose(PFindNOde(g_APPathList[i]));
  end;
  g_APPathList.Clear;

  if Root = nil then begin
    FreeTree();                         //�ڴ�й¶???
    Exit;
  end;

  New(Temp);
  Temp.X := Root.X;
  Temp.Y := Root.Y;
  g_APPathList.Add(Temp);

  dir := Root.dir;
  P := Root;
  Root := Root.Father;
  while (Root <> nil) do begin
    if dir <> Root.dir then begin
      New(Temp);
      Temp.X := P.X;
      Temp.Y := P.Y;
      g_APPathList.Insert(0, Temp);
      dir := Root.dir;
    end;
    P := Root;
    Root := Root.Father;
  end;
  FreeTree();
end;

function RandomRange(const AFrom, ATo: Integer): Integer;
begin
  if AFrom > ATo then
    Result := Random(AFrom - ATo) + ATo
  else
    Result := Random(ATo - AFrom) + AFrom;
end;

procedure Init_Queue();
var
  i                         : Integer;
begin
  FreeTree();                           //�ڴ�й¶???
  if g_APQueue <> nil then begin
    if g_APQueue.Next <> nil then Dispose(g_APQueue.Next);
    g_APQueue.Next := nil;
    if g_APQueue.Node <> nil then Dispose(g_APQueue.Node);
    g_APQueue.Node := nil;
    Dispose(g_APQueue);
    g_APQueue := nil;
  end;

  New(g_APQueue);
  g_APQueue.Node := nil;
  g_APQueue.F := -1;
  New(g_APQueue.Next);
  g_APQueue.Next.F := $FFFFFFF;
  g_APQueue.Next.Node := nil;
  g_APQueue.Next.Next := nil;

  for i := g_APPathList.count - 1 downto 0 do begin
    Dispose(PFindNOde(g_APPathList[i]));
  end;
  g_APPathList.Clear;
end;

procedure Init_Queue2();
var
  i                         : Integer;
begin
  FreeTree();                           //�ڴ�й¶???
  if g_APQueue <> nil then begin
    if g_APQueue.Next <> nil then Dispose(g_APQueue.Next);
    g_APQueue.Next := nil;
    if g_APQueue.Node <> nil then Dispose(g_APQueue.Node);
    g_APQueue.Node := nil;
    Dispose(g_APQueue);
    g_APQueue := nil;
  end;
  for i := g_APPathList.count - 1 downto 0 do begin
    Dispose(PFindNOde(g_APPathList[i]));
  end;
  g_APPathList.Clear;
end;

function IsBackToSafeZone(var ret: Integer): Boolean;
var
  i                         : Integer;
  has                       : Boolean;
begin
  Result := False;
  ret := 0;
  if g_gcAss[1] then begin              //��û�лس�
    has := False;
    for i := 0 to MAXBAGITEMCL - 1 do begin
      if (g_ItemArr[i].S.Name <> '') and (g_ItemArr[i].S.AC > 0) and (g_ItemArr[i].S.StdMode = 0) then begin
        has := True;
        Break;
      end;
    end;
    if not has then begin
      ret := 1;
      Result := True;
      Exit;
    end;
  end;

  if g_gcAss[2] then begin              //��û�лس�
    has := False;
    for i := 0 to MAXBAGITEMCL - 1 do begin
      if (g_ItemArr[i].S.Name <> '') and (g_ItemArr[i].S.MAC > 0) and (g_ItemArr[i].S.StdMode = 0) then begin
        has := True;
        Break;
      end;
    end;
    if not has then begin
      ret := 2;
      Result := True;
      Exit;
    end;
  end;

  //������û�лس�
  if g_gcAss[4] then begin
    has := False;
    for i := 0 to 45 do begin
      if (g_ItemArr[i].S.Name = '') then begin
        has := True;
        Break;
      end;
    end;
    if not has then begin
      ret := 3;
      Result := True;
      Exit;
    end;
  end;

  //��û�лس�
  if g_gcAss[3] then begin
    has := False;
    for i := 0 to MAXBAGITEMCL - 1 do begin
      if (g_ItemArr[i].S.StdMode = 25) and (g_ItemArr[i].S.Name <> '') and (Pos('��', g_ItemArr[i].S.Name) > 0) then begin
        has := True;
        Break;
      end;
    end;
    if not has then begin
      ret := 4;
      Result := True;
      Exit;
    end else begin
      has := False;
      for i := 0 to MAXBAGITEMCL - 1 do begin
        if (g_ItemArr[i].S.StdMode = 25) and (g_ItemArr[i].S.Name <> '') and (Pos('ҩ', g_ItemArr[i].S.Name) > 0) then begin
          has := True;
          Break;
        end;
      end;
      if not has then begin
        ret := 5;
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function GetDis(X1, Y1, X2, Y2: Integer): Integer;
begin
  Result := (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2);
end;

function IsProperTarget(Actor: TActor): Boolean; //(Actor.m_nHiterCode = 0) and
begin
  Result := (Actor <> nil) and
    (Actor.m_btRace <> 0) and
    (Actor.m_sUserName <> '') and
    (not (Actor.m_btRace in [12, 50])) and
    (not Actor.m_boDeath) and
    (Actor.m_btRace <> 12) and
    ((Actor.m_nState and STATE_STONE_MODE) = 0) and
    (Pos('(', Actor.m_sUserName) = 0) and
    (Actor.m_boVisible) and
    (not Actor.m_boDelActor) and
    (not Actor.m_btAFilter) and
    (g_gcAss[6] and (g_APMobList.IndexOf(Actor.m_sUserName) < 0));
end;

function SearchTarget(): TActor;
var
  i                         : Integer;
  Actor                     : TActor;
  dx, distance              : Integer;
begin
  Result := nil;

  distance := 10000;
  if g_APTagget <> nil then begin
    if (not g_APTagget.m_boDeath) and (g_APTagget.m_nHiterCode = g_MySelf.m_nRecogId) and (g_APTagget.m_boVisible) and (not g_APTagget.m_boDelActor) then begin
      distance := GetDis(g_APTagget.m_nCurrX, g_APTagget.m_nCurrY, g_MySelf.m_nCurrX, g_MySelf.m_nCurrY);
      Result := g_APTagget;
      //Exit;
    end;
  end;

  with g_PlayScene do begin
    for i := 0 to m_ActorList.count - 1 do begin
      Actor := TActor(m_ActorList[i]);
      if IsProperTarget(Actor) then begin
        dx := GetDis(Actor.m_nCurrX, Actor.m_nCurrY, g_MySelf.m_nCurrX, g_MySelf.m_nCurrY);
        if dx < distance then begin
          distance := dx;
          Result := Actor;
        end;
      end;
    end;
  end;
end;

function GetDropItemsDis(): Integer;
var
  i, j, dx                  : Integer;
  d                         : pTDropItem;
begin
  Result := 100000;
  for i := 0 to g_DropedItemList.count - 1 do begin
    d := pTDropItem(g_DropedItemList[i]);
    if g_boPickUpAll or d.boPickUp then begin //���ʰȡ���ˣ����ж��Ƿ����
      dx := GetDis(d.X, d.Y, g_MySelf.m_nCurrX, g_MySelf.m_nCurrY); //��ȡ���룬ѡ�������
      if (dx < Result) and (dx <> 0) then begin
        g_AutoPicupItem := d;
        Result := dx;
      end;
    end;
  end;
end;

function GetAutoPalyStation(): Integer;
var
  has, bPcaketfull          : Boolean;
  i, Mobdistance, ItemDistance: Integer;
begin
  //�ж��Ƿ�س�
  //if GetTickCount - g_APRunTick >= 200 then begin
  //  g_APRunTick := GetTickCount;
  //end else
  //  exit;
  Result := 0;
  bPcaketfull := False;
  if IsBackToSafeZone(Mobdistance) then begin
    Result := 0;
    Exit;
  end else begin
    has := False;
    for i := 0 to 45 do begin
      if (g_ItemArr[i].S.Name = '') then begin
        has := True;
        Break;
      end;
    end;
    if not has then begin               //����
      bPcaketfull := True;
    end;
  end;

  if g_nOverAPZone > 0 then begin
    //Dec(g_nOverAPZone);
    Result := 4;
    Exit;
  end;

  if (g_APMapPath <> nil) and (g_APStep >= 0) and (g_APStep <= High(g_APMapPath)) then begin
    //����ѭ·��������Χ������
    if g_APLastPoint.X >= 0 then begin
      if (((abs(g_APLastPoint.X - g_MySelf.m_nCurrX) >= overdisc) or (abs(g_APLastPoint.Y - g_MySelf.m_nCurrY) >= overdisc))) and
        ((abs(g_APMapPath[g_APStep].X - g_MySelf.m_nCurrX) >= overdisc) or (abs(g_APMapPath[g_APStep].Y - g_MySelf.m_nCurrY) >= overdisc)) then begin
        g_nOverAPZone := 14;
        Result := 4;
        Exit;
      end;
    end else begin
      if ((abs(g_APMapPath[g_APStep].X - g_MySelf.m_nCurrX) >= overdisc) or (abs(g_APMapPath[g_APStep].Y - g_MySelf.m_nCurrY) >= overdisc)) then begin
        g_nOverAPZone := 14;
        Result := 4;
        Exit;
      end;
    end;
  end;

  //��ȡ����Ĺ���
  if (g_APTagget <> nil) then begin
    if g_APTagget.m_boDelActor or g_APTagget.m_boDeath then
      //or (abs(g_APTagget.m_nCurrX - g_MySelf.m_nCurrX) > 15) or (abs(g_APTagget.m_nCurrY - g_MySelf.m_nCurrY) > 15) then
      g_APTagget := nil;
  end;
  if ((GetTickCount - g_dwSearchEnemyTick) > 4000) or (((GetTickCount - g_dwSearchEnemyTick) > 300) and ((g_APTagget = nil))) then begin
    g_dwSearchEnemyTick := GetTickCount();
    g_APTagget := SearchTarget();
  end;
  if (g_APTagget <> nil) then begin
    if g_APTagget.m_boDelActor or g_APTagget.m_boDeath then
      //or (abs(g_APTagget.m_nCurrX - g_MySelf.m_nCurrX) > 15) or (abs(g_APTagget.m_nCurrY - g_MySelf.m_nCurrY) > 15) then
      g_APTagget := nil;
  end;

  if g_APTagget <> nil then
    Mobdistance := {GetDistance} GetDis(g_APTagget.m_nCurrX, g_APTagget.m_nCurrY, g_MySelf.m_nCurrX, g_MySelf.m_nCurrY)
  else
    Mobdistance := 100000;

  //��ȡ�������Ʒ
  if not bPcaketfull then begin
    ItemDistance := GetDropItemsDis;
  end else
    g_AutoPicupItem := nil;

  //���߶�û�з���
  if (ItemDistance = 100000) and ((Mobdistance = 100000) or (Mobdistance = 0)) then begin //û�з��ֹ������Ʒ�������
    Result := 3;
    Exit;
  end;

  if (ItemDistance + 2) >= Mobdistance then //����ɱ��
    Result := 1                         //���ֹ���
  else
    Result := 2;                        //������Ʒ
end;

function AutoUseMagic(Key: Byte; target: TActor; nx: Integer; ny: Integer): Boolean;
var
  pcm                       : PTClientMagic;
  pmag                      : PTUseMagicInfo;
  tdir                      : Integer;
begin
  Result := True;
  pcm := frmMain.GetMagicByID(Key);     //frmMain.GetMagicByKey(Char(Key + Byte('1')));
  if pcm = nil then begin
    Result := False;
    Exit;
  end;
  g_FocusCret := target;
  if nx >= 0 then begin
    frmMain.UseMagic(nx, ny, pcm, True);
  end else begin
    frmMain.UseMagic(target.m_nCurrX, target.m_nCurrY, pcm);
  end;
end;

function TargetCount(target: TActor): Integer;
var
  rx, ry                    : Integer;
  Actor                     : TActor;
begin
  Result := 1;

  rx := target.m_nCurrX + 1;
  ry := target.m_nCurrY;
  Actor := g_PlayScene.FindActorXY(rx, ry);
  if IsProperTarget(Actor) then Inc(Result);

  rx := target.m_nCurrX + 1;
  ry := target.m_nCurrY + 1;
  Actor := g_PlayScene.FindActorXY(rx, ry);
  if IsProperTarget(Actor) then Inc(Result);
  //if Result > 2 then Exit;

  rx := target.m_nCurrX + 1;
  ry := target.m_nCurrY - 1;
  Actor := g_PlayScene.FindActorXY(rx, ry);
  if IsProperTarget(Actor) then Inc(Result);
  //if Result > 2 then Exit;

  rx := target.m_nCurrX - 1;
  ry := target.m_nCurrY;
  Actor := g_PlayScene.FindActorXY(rx, ry);
  if IsProperTarget(Actor) then Inc(Result);
  //if Result > 2 then Exit;

  rx := target.m_nCurrX - 1;
  ry := target.m_nCurrY + 1;
  Actor := g_PlayScene.FindActorXY(rx, ry);
  if IsProperTarget(Actor) then Inc(Result);
  //if Result > 2 then Exit;

  rx := target.m_nCurrX - 1;
  ry := target.m_nCurrY - 1;
  Actor := g_PlayScene.FindActorXY(rx, ry);
  if IsProperTarget(Actor) then Inc(Result);
  //if Result > 2 then Exit;

  rx := target.m_nCurrX;
  ry := target.m_nCurrY + 1;
  Actor := g_PlayScene.FindActorXY(rx, ry);
  if IsProperTarget(Actor) then Inc(Result);
  //if Result > 2 then Exit;

  rx := target.m_nCurrX;
  ry := target.m_nCurrY - 1;
  Actor := g_PlayScene.FindActorXY(rx, ry);
  if IsProperTarget(Actor) then Inc(Result);
  //if Result > 2 then Exit;

end;

function TargetCount2(target: TActor): Integer;
var
  i, rx, ry                 : Integer;
  Actor                     : TActor;
begin
  Result := 0;
  with g_PlayScene do begin
    for i := 0 to m_ActorList.count - 1 do begin
      Actor := TActor(m_ActorList[i]);
      if (abs(Actor.m_nCurrX - g_MySelf.m_nCurrX) < 6) or (abs(Actor.m_nCurrY - g_MySelf.m_nCurrY) < 6) then begin
        if IsProperTarget(Actor) then begin
          Inc(Result);
        end;
      end;
    end;
  end;
end;

function TargetCount3(target: TActor): Integer;
var
  i, rx, ry                 : Integer;
  Actor                     : TActor;
begin
  Result := 0;
  with g_PlayScene do begin
    for i := 0 to m_ActorList.count - 1 do begin
      Actor := TActor(m_ActorList[i]);
      if (abs(Actor.m_nCurrX - g_MySelf.m_nCurrX) < 5) or (abs(Actor.m_nCurrY - g_MySelf.m_nCurrY) < 5) then begin
        if IsProperTarget(Actor) then begin
          Inc(Result);
        end;
      end;
    end;
  end;
end;

function TargetHumCount(target: TActor): Integer;
var
  b                         : Boolean;
  i, rx, ry                 : Integer;
  Actor                     : TActor;
begin
  Result := 0;
  with g_PlayScene do begin
    for i := 0 to m_ActorList.count - 1 do begin
      Actor := TActor(m_ActorList[i]);
      if (abs(Actor.m_nCurrX - g_MySelf.m_nCurrX) < 8) or (abs(Actor.m_nCurrY - g_MySelf.m_nCurrY) < 8) then begin
        b := (Actor <> nil) and (not Actor.m_boDeath) and ((Actor.m_btRace = 0) or (Actor.m_btIsHero = 1));
        if b then begin
          Inc(Result);
        end;
      end;
    end;
  end;
end;

function XPATTACK(): Boolean;
var
  pcm                       : PTClientMagic;
begin
  Result := False;
  if g_MySelf.m_HeroObject <> nil then begin
    if (g_MySelf.m_HeroObject.m_nHeroEnergyType <> 2) and (g_MySelf.m_HeroObject.m_nHeroEnergy > 0) and (g_MySelf.m_HeroObject.m_nHeroEnergy >= g_MySelf.m_HeroObject.m_nMaxHeroEnergy) then begin
      if GetTickCount - g_dwLatestJoinAttackTick <= 3000 then Exit;
      pcm := nil;
      case g_MySelf.m_btJob of
        0: begin
            case g_MySelf.m_HeroObject.m_btJob of
              0: pcm := frmMain.HeroGetMagicByID(60);
              1: pcm := frmMain.HeroGetMagicByID(62);
              2: pcm := frmMain.HeroGetMagicByID(61);
            end;
            if (pcm <> nil) and frmMain.CanNextAction and frmMain.ServerAcceptNextAction and frmMain.CanNextHit then begin
              g_dwLatestSpellTick := GetTickCount;
              g_dwLastMoveTick := GetTickCount;
              Result := True;
              g_FocusCret := g_APTagget;
              frmMain.SendHeroSetTarget;
              frmMain.SendHeroJoinAttack();
              g_dwLatestJoinAttackTick := GetTickCount;
              Exit;
            end;
          end;
        1: if CanNextSpell() then begin
            case g_MySelf.m_HeroObject.m_btJob of
              0: pcm := frmMain.HeroGetMagicByID(62);
              1: pcm := frmMain.HeroGetMagicByID(65);
              2: pcm := frmMain.HeroGetMagicByID(64);
            end;
          end;
        2: if CanNextSpell() then begin
            case g_MySelf.m_HeroObject.m_btJob of
              0: pcm := frmMain.HeroGetMagicByID(61);
              1: pcm := frmMain.HeroGetMagicByID(64);
              2: pcm := frmMain.HeroGetMagicByID(63);
            end;
          end;
      end;
      if (pcm <> nil) and frmMain.CanNextAction and frmMain.ServerAcceptNextAction then begin
        g_dwLatestSpellTick := GetTickCount;
        g_dwLastMoveTick := GetTickCount;
        Result := True;
        g_FocusCret := g_APTagget;
        frmMain.SendHeroSetTarget;
        frmMain.SendHeroJoinAttack();
        g_dwLatestJoinAttackTick := GetTickCount;
      end;
    end;
  end;
end;

function HeroAttackTagget(target: TActor): Boolean;
var
  attackOK                  : Boolean;
  n, m, tdir, dx, dy, hitmsg, MagicKey: Integer;
  NextHitTime, LevelFastTime: Integer;
  pcm                       : PTClientMagic;

  i, nspeed                 : Integer;
  boTrainOk                 : Boolean;
  nTag, nx, ny, nAbsX, nAbsY: Integer;
  nNX, nNY, nTX, nTY, nOldDC: Integer;
  dwAttackTime              : LongWord;
label
  AAAA, BBBB, CCCC, DDDD, EEEE, FFFF;

begin
  Result := False;
  g_boAPAutoMove := False;

  g_nTagCount := 0;
  if (g_MySelf = nil) or g_MySelf.m_boDeath or (g_APTagget = nil) or g_APTagget.m_boDeath then Exit;

  attackOK := False;
  case g_MySelf.m_btJob of
    0: begin
        //
        if g_SeriesSkillReady then begin
          frmMain.SendFireSerieSkill();
          //Result := True;
        end;
        if g_gcTec[4] and (g_MySelf.m_nState and $00100000 = 0) and CanNextSpell()   then begin
          if g_MagicArr[0][31] <> nil then begin
            frmMain.UseMagic(SCREENWIDTH div 2, SCREENHEIGHT div 2, g_MagicArr[0][31]);
            Exit;
          end;
        end;
        if XPATTACK() or frmMain.AttackTarget(g_APTagget) then begin
          Result := True;
          Exit;
        end;
      end;
    1: begin
        if g_MySelf.m_Abil.Level < 7 then begin
          if frmMain.AttackTarget(g_APTagget) then
            Result := True;
          Exit;
        end;
        //
        if g_gcTec[4] and (g_MySelf.m_nState and $00100000 = 0) and CanNextSpell()  then begin
          if g_MagicArr[0][31] <> nil then begin
            frmMain.UseMagic(SCREENWIDTH div 2, SCREENHEIGHT div 2, g_MagicArr[0][31]);
            Exit;
          end;
        end;

        if g_SeriesSkillReady and (g_MagicLockActor <> nil) and (not g_MagicLockActor.m_boDeath) then begin
          frmMain.SendFireSerieSkill();
        end;
        if XPATTACK() then begin
          Result := True;
          Exit;
        end;

        MagicKey := 11;
        nAbsX := abs(g_MySelf.m_nCurrX - g_APTagget.m_nCurrX);
        nAbsY := abs(g_MySelf.m_nCurrY - g_APTagget.m_nCurrY);
        if ((nAbsX > 2) or (nAbsY > 2)) then begin
          if (nAbsX <= g_nMagicRange) and (nAbsY <= g_nMagicRange) then begin
            Result := True;
            g_sAPStr := Format('[�һ�] ����Ŀ�꣺%s (%d,%d) ����ʹ��ħ������', [g_APTagget.m_sUserName, g_APTagget.m_nCurrX, g_APTagget.m_nCurrY]);
            if frmMain.CanNextAction and frmMain.ServerAcceptNextAction then begin

              if CanNextSpell() then begin

                if (g_MagicArr[0][22] <> nil) then begin
                  if TargetCount3(g_APTagget) >= 15 then begin
                    tdir := GetNextDirection(g_APTagget.m_nCurrX, g_APTagget.m_nCurrY, g_MySelf.m_nCurrX, g_MySelf.m_nCurrY);
                    GetFrontPosition(g_APTagget.m_nCurrX, g_APTagget.m_nCurrY, tdir, nx, ny);
                    GetFrontPosition(nx, ny, tdir, nx, ny);
                    if EventMan.GetEvent(nx, ny, ET_FIRE) = nil then begin
                      m_dwTargetFocusTick := GetTickCount();
                      if AutoUseMagic(22, g_APTagget, nx, ny) then
                        Exit
                      else begin
                        Result := False;
                        Exit;
                      end;
                    end;
                  end;
                end;

                nOldDC := 3;
                FFFF:
                if (g_MagicArr[0][10] <> nil) then begin
                  tdir := GetNextDirection(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, g_APTagget.m_nCurrX, g_APTagget.m_nCurrY);
                  if GetNextPosition(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, 1, nNX, nNY) then begin
                    GetNextPosition(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, 8, nTX, nTY);
                    if CheckMagPassThrough(nNX, nNY, nTX, nTY, tdir) >= nOldDC then begin
                      m_dwTargetFocusTick := GetTickCount();
                      MagicKey := 10;
                      goto AAAA;
                    end;
                  end;
                end;
                if (g_MagicArr[0][9] <> nil) then begin
                  tdir := GetNextDirection(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, g_APTagget.m_nCurrX, g_APTagget.m_nCurrY);
                  if GetNextPosition(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, 1, nNX, nNY) then begin
                    GetNextPosition(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, 5, nTX, nTY);
                    if CheckMagPassThrough(nNX, nNY, nTX, nTY, tdir) >= nOldDC then begin
                      m_dwTargetFocusTick := GetTickCount();
                      MagicKey := 9;
                      goto AAAA;
                    end;
                  end;
                end;

                if m_btMagPassTh > 0 then begin
                  Dec(m_btMagPassTh);
                  nOldDC := 1;
                  goto FFFF;
                end;

                if g_MagicArr[0][11] <> nil then
                  MagicKey := 11
                else if g_MagicArr[0][5] <> nil then
                  MagicKey := 5
                else if g_MagicArr[0][1] <> nil then
                  MagicKey := 1;

                g_nTagCount := TargetCount(g_APTagget);

                if g_nTagCount >= 2 then begin
                  if Random(7) > 1 then begin
                    if (g_MagicArr[0][58] <> nil) and (Random(8) > 1) then begin
                      MagicKey := 58;
                    end else if (g_MagicArr[0][33] <> nil) then
                      MagicKey := 33;
                  end else if (g_MagicArr[0][47] <> nil) then
                    MagicKey := 47;
                  if (MagicKey <= 11) and (g_MagicArr[0][23] <> nil) then
                    MagicKey := 23;
                end;

                AAAA:
                if AutoUseMagic(MagicKey, g_APTagget) then
                  Exit
                else begin
                  Result := False;
                  Exit;
                end;
              end;
            end;
          end else begin
            Result := False;
            Exit;
          end;
        end else begin
          if ((nAbsX <= 1) and (nAbsY <= 1)) then begin //Ŀ�����
            if CanNextSpell() then begin
              nTag := TargetCount(g_MySelf);
              if (nTag >= 5) then begin //��̫��,ǿ����Χ...

                DDDD:
                if CanNextSpell() then begin
                  MagicKey := 0;
                  if Random(7) > 1 then begin
                    if (g_MagicArr[0][58] <> nil) and (Random(8) > 1) then begin
                      MagicKey := 58;
                    end else if (g_MagicArr[0][33] <> nil) then
                      MagicKey := 33;
                  end else if (g_MagicArr[0][47] <> nil) then
                    MagicKey := 47;
                  if (MagicKey <= 11) and (g_MagicArr[0][23] <> nil) then
                    MagicKey := 23;
                  if MagicKey > 0 then begin
                    Result := True;
                    goto AAAA;
                  end;

                  if g_MagicArr[0][11] <> nil then
                    MagicKey := 11
                  else if g_MagicArr[0][5] <> nil then
                    MagicKey := 5
                  else if g_MagicArr[0][1] <> nil then
                    MagicKey := 1;
                  if MagicKey > 0 then begin
                    Result := True;
                    goto AAAA;
                  end;
                end;
                Result := False;
                Exit;                   //���
              end;
              if (nTag >= 4) and (g_MagicArr[0][8] <> nil) then begin //�Ƚ���ǿ�Ŀ���...һ��ѡ���ӱ�
                Result := True;
                m_dwTargetFocusTick := GetTickCount();
                if AutoUseMagic(8, g_MySelf) then begin
                  if m_btMagPassTh <= 0 then Inc(m_btMagPassTh, 1 + Random(2));
                  Exit;
                end;
              end;
            end;
          end;

          tdir := GetNextDirection(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, g_APTagget.m_nCurrX, g_APTagget.m_nCurrY); //�ܹ�
          GetBackPosition(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, nx, ny);
          nTag := 0;
          while True do begin
            if g_PlayScene.CanWalk(nx, ny) then Break;
            Inc(tdir);
            tdir := tdir mod 8;
            GetBackPosition(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, nx, ny);
            Inc(nTag);
            if nTag > 8 then Break;
          end;
          if g_PlayScene.CanWalk(nx, ny) then begin
            GetBackPosition2(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, nTX, nTY);
            if g_PlayScene.CanWalk(nTX, nTY) {Map.CanMove(nTX, nTY)} then begin
              //DScreen.AddChatBoardString(Format('�ܹ�2(%d:%d)...........', [nTX, nTY]), clBlue, clWhite);
              g_nTargetX := nTX;
              g_nTargetY := nTY;
              g_ChrAction := caRun;
              g_nMouseCurrX := nTX;
              g_nMouseCurrY := nTY;
              Result := True;
            end else begin
              //DScreen.AddChatBoardString(Format('�ܹ�(%d:%d)...........', [nX, nY]), clBlue, clWhite);
              g_nTargetX := nx;
              g_nTargetY := ny;
              g_ChrAction := caRun;
              g_nMouseCurrX := nx;
              g_nMouseCurrY := ny;
              Result := True;
            end;
          end else begin                //ǿ��
            //DScreen.AddChatBoardString('ǿ��...........', clBlue, clWhite);
            nTag := 4;
            goto DDDD;
          end;
        end;
      end;

    2: begin                            ////////////////////////////////////////
        if g_gcTec[4] and (g_MySelf.m_nState and $00100000 = 0) and CanNextSpell()  then begin
          if g_MagicArr[0][31] <> nil then begin
            frmMain.UseMagic(SCREENWIDTH div 2, SCREENHEIGHT div 2, g_MagicArr[0][31]);
            Exit;
          end;
        end;

        n := 0;
        if (g_UseItems[U_ARMRINGL].S.StdMode = 25) and
          (g_UseItems[U_ARMRINGL].S.Shape <> 6) and
          (Pos('ҩ', g_UseItems[U_ARMRINGL].S.Name) > 0) then begin
          Inc(n);
        end;
        if n = 0 then begin
          for i := 6 to MAXBAGITEMCL - 1 do begin
            if (g_ItemArr[i].S.NeedIdentify < 4) and
              (g_ItemArr[i].S.StdMode = 25) and
              (g_ItemArr[i].S.Shape <> 6) and
              (Pos('ҩ', g_ItemArr[i].S.Name) > 0) then begin
              Inc(n);
              Break;
            end;
          end;
        end;
        if n = 0 then begin
          if GetTickCount - g_hinttick1 > 60 * 1000 then begin
            g_hinttick1 := GetTickCount;
            DScreen.AddChatBoardString('���[ҩ��]�Ѿ����꣬ע�ⲹ��', clWhite, clBlue);
          end;
        end;

        m := 0;
        if (g_UseItems[U_ARMRINGL].S.StdMode = 25) and
          (g_UseItems[U_ARMRINGL].S.Shape <> 6) and
          (Pos('��', g_UseItems[U_ARMRINGL].S.Name) > 0) then begin
          Inc(m);
        end;
        if m = 0 then begin
          for i := 6 to MAXBAGITEMCL - 1 do begin
            if (g_ItemArr[i].S.NeedIdentify < 4) and
              (g_ItemArr[i].S.StdMode = 25) and
              (g_ItemArr[i].S.Shape <> 6) and
              (Pos('��', g_ItemArr[i].S.Name) > 0) then begin
              Inc(m);
              Break;
            end;
          end;
        end;
        if m = 0 then begin
          if GetTickCount - g_hinttick2 > 60 * 1000 then begin
            g_hinttick2 := GetTickCount;
            DScreen.AddChatBoardString('���[�����]�Ѿ����꣬ע�ⲹ��', clWhite, clBlue);
          end;
        end;

        if (GetTickCount - m_dwRecallTick) > (1000 * 6) then begin //���ñȽϴ�ʱ��,�Ա���������...
          m_dwRecallTick := GetTickCount();
          if (g_MySelf.m_SlaveObject.count = 0) and (m > 0) then begin
            MagicKey := 0;
            if (g_MagicArr[0][55] <> nil) then
              MagicKey := 55
            else if (g_MagicArr[0][30] <> nil) then
              MagicKey := 30
            else if (g_MagicArr[0][17] <> nil) then
              MagicKey := 17;

            if (MagicKey <> 0) then begin
              Result := True;
              pcm := frmMain.GetMagicByID(MagicKey);
              if pcm = nil then begin
                Result := False;
                Exit;
              end;
              g_FocusCret := nil;
              tdir := GetNextDirection(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, g_APTagget.m_nCurrX, g_APTagget.m_nCurrY);
              GetFrontPosition(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, nx, ny);
              frmMain.UseMagic(nx, ny, pcm, True);
              Exit;
            end;
          end;
        end;

        if (GetTickCount - m_dwSpellTick) > (1000 * 5) then begin //״̬��ħ��...
          m_dwSpellTick := GetTickCount();

          // MAGDEFENCEUP
          if (g_MagicArr[0][14] <> nil) and (m > 0) then begin
            if (g_MySelf.m_nState and $00200000 = 0) then begin
              Result := True;
              if AutoUseMagic(14, g_MySelf) then
                Exit;
            end;
            if g_MySelf.m_HeroObject <> nil then begin
              if (g_MySelf.m_HeroObject.m_nState and $00200000) = 0 then begin
                Result := True;
                if AutoUseMagic(14, g_MySelf.m_HeroObject) then
                  Exit;
              end;
            end;
          end;

          //  Double DefenceUp
          if (g_MagicArr[0][15] <> nil) and (m > 0) then begin
            if (g_MySelf.m_nState and $00400000) = 0 then begin
              Result := True;
              if AutoUseMagic(15, g_MySelf) then
                Exit;
            end;
            if g_MySelf.m_HeroObject <> nil then begin
              if (g_MySelf.m_HeroObject.m_nState and $00400000 = 0) then begin
                Result := True;
                if AutoUseMagic(15, g_MySelf.m_HeroObject) then
                  Exit;
              end;
            end;
          end;

          //  Healling
          if (g_MagicArr[0][2] <> nil) then begin
            if (Round((g_MySelf.m_Abil.HP / g_MySelf.m_Abil.MaxHP) * 100) < 85) then begin
              Result := True;
              if AutoUseMagic(2, g_MySelf) then
                Exit;
            end;
            if g_MySelf.m_HeroObject <> nil then begin
              if (g_MySelf.m_HeroObject.m_Abil.HP <> 0) and (abs(g_MySelf.m_nCurrX - g_MySelf.m_HeroObject.m_nCurrX + 2) <= g_nMagicRange) and (abs(g_MySelf.m_nCurrY - g_MySelf.m_HeroObject.m_nCurrY + 2) <= g_nMagicRange) then begin
                if (Round((g_MySelf.m_HeroObject.m_Abil.HP / g_MySelf.m_HeroObject.m_Abil.MaxHP) * 100) <= 80) then begin
                  Result := True;
                  if AutoUseMagic(2, g_MySelf.m_HeroObject) then
                    Exit;
                end;
              end;
            end;
            if g_MySelf.m_SlaveObject.count > 0 then begin
              for i := 0 to g_MySelf.m_SlaveObject.count - 1 do begin
                if TActor(g_MySelf.m_SlaveObject[i]).m_boDeath then Continue;
                if (TActor(g_MySelf.m_SlaveObject[i]).m_Abil.HP <> 0) and (abs(g_MySelf.m_nCurrX - TActor(g_MySelf.m_SlaveObject[i]).m_nCurrX + 2) <= g_nMagicRange) and (abs(g_MySelf.m_nCurrY - TActor(g_MySelf.m_SlaveObject[i]).m_nCurrY + 2) <= g_nMagicRange) then begin
                  if (Round((TActor(g_MySelf.m_SlaveObject[i]).m_Abil.HP / TActor(g_MySelf.m_SlaveObject[i]).m_Abil.MaxHP) * 100) <= 80) then begin
                    Result := True;
                    if AutoUseMagic(2, TActor(g_MySelf.m_SlaveObject[i])) then
                      Exit;
                  end;
                end;
              end;
            end;
          end;
        end;

        if (g_MySelf.m_Abil.Level < 18) or (g_MagicArr[0][13] = nil) or ((n = 0) and (m = 0)) then begin
          CCCC:
          if (GetTickCount - m_dwSpellTick) > (3000) then begin
            m_dwSpellTick := GetTickCount();

            if (g_MagicArr[0][2] <> nil) then begin
              Result := True;
              if (Round((g_MySelf.m_Abil.HP / g_MySelf.m_Abil.MaxHP) * 100) < 85) then begin
                if AutoUseMagic(2, g_MySelf) then
                  Exit;
              end;
              if g_MySelf.m_HeroObject <> nil then begin
                if (g_MySelf.m_HeroObject.m_Abil.HP <> 0) and (abs(g_MySelf.m_nCurrX - g_MySelf.m_HeroObject.m_nCurrX + 2) <= g_nMagicRange) and (abs(g_MySelf.m_nCurrY - g_MySelf.m_HeroObject.m_nCurrY + 2) <= g_nMagicRange) then begin
                  if (Round((g_MySelf.m_HeroObject.m_Abil.HP / g_MySelf.m_HeroObject.m_Abil.MaxHP) * 100) < 85) then begin
                    if AutoUseMagic(2, g_MySelf.m_HeroObject) then
                      Exit;
                  end;
                end;
              end;
              for i := 0 to g_MySelf.m_SlaveObject.count - 1 do begin
                if TActor(g_MySelf.m_SlaveObject[i]).m_boDeath then Continue;
                if (abs(g_MySelf.m_nCurrX - TActor(g_MySelf.m_SlaveObject[i]).m_nCurrX + 2) <= g_nMagicRange) and (abs(g_MySelf.m_nCurrY - TActor(g_MySelf.m_SlaveObject[i]).m_nCurrY + 2) <= g_nMagicRange) then begin
                  if (TActor(g_MySelf.m_SlaveObject[i]).m_Abil.HP <> 0) and (Round((TActor(g_MySelf.m_SlaveObject[i]).m_Abil.HP / TActor(g_MySelf.m_SlaveObject[i]).m_Abil.MaxHP) * 100) < 85) then begin
                    if AutoUseMagic(2, TActor(g_MySelf.m_SlaveObject[i])) then
                      Exit;
                  end;
                end;
              end;
            end;
          end;

          if frmMain.AttackTarget(g_APTagget) then
            Result := True;
          Exit;
        end;

        if g_SeriesSkillReady and (g_MagicLockActor <> nil) and (not g_MagicLockActor.m_boDeath) then begin
          frmMain.SendFireSerieSkill();
        end;
        if XPATTACK() then begin
          Result := True;
          Exit;
        end;

        MagicKey := 0;
        nAbsX := abs(g_MySelf.m_nCurrX - g_APTagget.m_nCurrX);
        nAbsY := abs(g_MySelf.m_nCurrY - g_APTagget.m_nCurrY);
        if ((nAbsX > 2) or (nAbsY > 2)) then begin //��Ҫ���ټ����...
          if (nAbsX <= g_nMagicRange) and (nAbsY <= g_nMagicRange) then begin
            Result := True;
            g_sAPStr := Format('[�һ�] ����Ŀ�꣺%s (%d,%d) ����ʹ��ħ������', [g_APTagget.m_sUserName, g_APTagget.m_nCurrX, g_APTagget.m_nCurrY]);
            if frmMain.CanNextAction and frmMain.ServerAcceptNextAction then begin
              EEEE:

              if CanNextSpell() then begin
                //  DoubluSC
                if (g_MagicArr[0][50] <> nil) then begin
                  if GetTickCount - m_dwDoubluSCTick > 90 * 1000 then begin
                    m_dwDoubluSCTick := GetTickCount();
                    m_dwTargetFocusTick := GetTickCount();
                    MagicKey := 50;
                    goto BBBB;
                  end;
                end;

                //DECHEALTH & DAMAGEARMOR
                if GetTickCount - m_dwPoisonTick > 3500 then begin
                  m_dwPoisonTick := GetTickCount;
                  if (g_MagicArr[0][6] <> nil) then begin
                    if (g_APTagget.m_nState and $80000000 = 0) or (g_APTagget.m_nState and $40000000 = 0) then begin
                      m_dwTargetFocusTick := GetTickCount();
                      MagicKey := 6;
                      goto BBBB;
                    end;
                  end;
                  if (g_MagicArr[0][18] <> nil) and (g_MySelf.m_nState and $00800000 = 0) and (Random(4) = 0) then begin
                    if (TargetCount2(g_MySelf) >= 7) then begin
                      m_dwTargetFocusTick := GetTickCount();
                      MagicKey := 18;
                      goto BBBB;
                    end;
                  end;
                end;

                if (g_MagicArr[0][13] <> nil) or (g_MagicArr[0][57] <> nil) then begin
                  if (g_MagicArr[0][57] <> nil) and (((Round((g_MySelf.m_Abil.HP / g_MySelf.m_Abil.MaxHP) * 100) < 80) and (Random(100 - Round((g_MySelf.m_Abil.HP / g_MySelf.m_Abil.MaxHP) * 100)) > 5)) or (Random(10) > 6)) then begin
                    m_dwTargetFocusTick := GetTickCount();
                    MagicKey := 57;
                    goto BBBB;
                  end;
                  if (g_MagicArr[0][13] <> nil) then begin
                    m_dwTargetFocusTick := GetTickCount();
                    MagicKey := 13;
                    goto BBBB;
                  end;
                end;

                BBBB:
                if MagicKey > 0 then begin
                  if AutoUseMagic(MagicKey, g_APTagget) then
                    Exit;
                end else begin
                  Result := False;
                  goto CCCC;
                end;
              end;
            end;
          end else begin
            Result := False;
            Exit;
          end;
        end else begin
          if ((nAbsX <= 1) and (nAbsY <= 1)) then begin //Ŀ�����

            if CanNextSpell() then begin
              nTag := TargetCount(g_MySelf);
              if (nTag >= 5) then begin //��̫��,ǿ����Χ...
                goto EEEE;
              end;
              if (g_MagicArr[0][48] <> nil) and (nTag >= 3) then begin //�����Ŀ���...��ͬ�ڷ�ʦ�ӱ�
                Result := True;
                m_dwTargetFocusTick := GetTickCount();
                if AutoUseMagic(48, g_MySelf) then begin
                  if m_btMagPassTh <= 0 then Inc(m_btMagPassTh, 1 + Random(2));
                  Exit;
                end;
              end;
            end;
          end;

          tdir := GetNextDirection(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, g_APTagget.m_nCurrX, g_APTagget.m_nCurrY); //�ܹ�
          GetBackPosition(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, nx, ny);
          nTag := 0;
          while True do begin
            if g_PlayScene.CanWalk(nx, ny) then Break;
            Inc(tdir);
            tdir := tdir mod 8;
            GetBackPosition(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, nx, ny);
            Inc(nTag);
            if nTag > 8 then Break;
          end;
          if g_PlayScene.CanWalk(nx, ny) then begin
            GetBackPosition2(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, tdir, nTX, nTY);
            if g_PlayScene.CanWalk(nTX, nTY) {Map.CanMove(nTX, nTY)} then begin
              //DScreen.AddChatBoardString(Format('�ܹ�2(%d:%d)...........', [nTX, nTY]), clBlue, clWhite);
              g_nTargetX := nTX;
              g_nTargetY := nTY;
              g_ChrAction := caRun;
              g_nMouseCurrX := nTX;
              g_nMouseCurrY := nTY;
              Result := True;
            end else begin
              //DScreen.AddChatBoardString(Format('�ܹ�(%d:%d)...........', [nX, nY]), clBlue, clWhite);
              g_nTargetX := nx;
              g_nTargetY := ny;
              g_ChrAction := caRun;
              g_nMouseCurrX := nx;
              g_nMouseCurrY := ny;
              Result := True;
            end;
          end else begin                //ǿ��
            //DScreen.AddChatBoardString('ǿ��...........', clBlue, clWhite);
            goto EEEE;
          end;

        end;
      end;
  end;
end;

end.

