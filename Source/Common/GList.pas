unit GList;

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  TQuickID = record
    sAccount: string[20];
    sChrName: string[30];
    nIndex: Integer;
    nSelectID: Integer;
  end;
  pTQuickID = ^TQuickID;

  TGList = class(TList)
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
  end;

  TGStringList = class(TStringList)
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
  end;

  TQuickList = class(TStringList)
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SortString(nMIN, nMax: Integer);
    function GetIndex(sName: string): Integer;
    function AddRecord(sName: string; nIndex: Integer): Boolean;
    procedure Lock;
    procedure UnLock;
  end;

  TQuickIDList = class(TStringList)
  public
    procedure AddRecord(sAccount, sChrName: string; nIndex: Integer);
    procedure DelRecord(nIndex: Integer; sChrName: string);
    function GetChrList(sAccount: string; var ChrNameList: TList): Integer;
  end;

implementation

{ TGList }

constructor TGList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TGList.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TGList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TGList.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

{ TGStringList }

constructor TGStringList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TGStringList.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TGStringList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TGStringList.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

{ TQuickList }

constructor TQuickList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TQuickList.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TQuickList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TQuickList.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

function TQuickList.GetIndex(sName: string): Integer;
var
  nLow, nHigh, nMed, nCompareVal: Integer;
begin
  Result := -1;
  if self.Count <> 0 then begin
    if self.Sorted then begin
      if self.Count = 1 then begin
        if CompareStr(sName, self.Strings[0]) = 0 then
          Result := 0;
      end else begin
        nLow := 0;
        nHigh := self.Count - 1;
        nMed := (nHigh - nLow) div 2 + nLow;
        while (True) do begin
          if (nHigh - nLow) = 1 then begin
            if CompareStr(sName, self.Strings[nHigh]) = 0 then
              Result := nHigh;
            if CompareStr(sName, self.Strings[nLow]) = 0 then
              Result := nLow;
            break;
          end else begin
            nCompareVal := CompareStr(sName, self.Strings[nMed]);
            if nCompareVal > 0 then begin
              nLow := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            if nCompareVal < 0 then begin
              nHigh := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            Result := nMed;
            break;
          end;
        end;
      end;
    end else begin
      if self.Count = 1 then begin
        if CompareText(sName, self.Strings[0]) = 0 then
          Result := 0;
      end else begin
        nLow := 0;
        nHigh := self.Count - 1;
        nMed := (nHigh - nLow) div 2 + nLow;
        while (True) do begin
          if (nHigh - nLow) = 1 then begin
            if CompareText(sName, self.Strings[nHigh]) = 0 then
              Result := nHigh;
            if CompareText(sName, self.Strings[nLow]) = 0 then
              Result := nLow;
            break;
          end else begin
            nCompareVal := CompareText(sName, self.Strings[nMed]);
            if nCompareVal > 0 then begin
              nLow := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            if nCompareVal < 0 then begin
              nHigh := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            Result := nMed;
            break;
          end;
        end;
      end;
    end;
  end;

end;

procedure TQuickList.SortString(nMIN, nMax: Integer);
var
  ntMin, ntMax              : Integer;
  s18                       : string;
begin
  if self.Count > 0 then
    while (True) do begin
      ntMin := nMIN;
      ntMax := nMax;
      s18 := self.Strings[(nMIN + nMax) shr 1];
      while (True) do begin
        while (CompareText(self.Strings[ntMin], s18) < 0) do
          Inc(ntMin);
        while (CompareText(self.Strings[ntMax], s18) > 0) do
          Dec(ntMax);
        if ntMin <= ntMax then begin
          self.Exchange(ntMin, ntMax);
          Inc(ntMin);
          Dec(ntMax);
        end;
        if ntMin > ntMax then
          break
      end;
      if nMIN < ntMax then
        SortString(nMIN, ntMax);
      nMIN := ntMin;
      if ntMin >= nMax then
        break;
    end;
end;

function TQuickList.AddRecord(sName: string; nIndex: Integer): Boolean;
var
  nLow, nHigh, nMed, nCompareVal: Integer;
begin
  Result := True;
  if self.Count = 0 then
    self.AddObject(sName, TObject(nIndex))
  else begin
    if self.Sorted then begin
      if self.Count = 1 then begin
        nMed := CompareStr(sName, self.Strings[0]);
        if nMed > 0 then
          self.AddObject(sName, TObject(nIndex))
        else begin
          if nMed < 0 then
            self.InsertObject(0, sName, TObject(nIndex));
        end;
      end else begin
        nLow := 0;
        nHigh := self.Count - 1;
        nMed := (nHigh - nLow) div 2 + nLow;
        while (True) do begin
          if (nHigh - nLow) = 1 then begin
            nMed := CompareStr(sName, self.Strings[nHigh]);
            if nMed > 0 then begin
              self.InsertObject(nHigh + 1, sName, TObject(nIndex));
              break;
            end else begin
              nMed := CompareStr(sName, self.Strings[nLow]);
              if nMed > 0 then begin
                self.InsertObject(nLow + 1, sName, TObject(nIndex));
                break;
              end else begin
                if nMed < 0 then begin
                  self.InsertObject(nLow, sName, TObject(nIndex));
                  break;
                end else begin
                  Result := False;
                  break;
                end;
              end;
            end;
          end else begin
            nCompareVal := CompareStr(sName, self.Strings[nMed]);
            if nCompareVal > 0 then begin
              nLow := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            if nCompareVal < 0 then begin
              nHigh := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            Result := False;
            break;
          end;
        end;
      end;
    end else begin
      if self.Count = 1 then begin
        nMed := CompareText(sName, self.Strings[0]);
        if nMed > 0 then
          self.AddObject(sName, TObject(nIndex))
        else begin
          if nMed < 0 then
            self.InsertObject(0, sName, TObject(nIndex));
        end;
      end else begin
        nLow := 0;
        nHigh := self.Count - 1;
        nMed := (nHigh - nLow) div 2 + nLow;
        while (True) do begin
          if (nHigh - nLow) = 1 then begin
            nMed := CompareText(sName, self.Strings[nHigh]);
            if nMed > 0 then begin
              self.InsertObject(nHigh + 1, sName, TObject(nIndex));
              break;
            end
            else begin
              nMed := CompareText(sName, self.Strings[nLow]);
              if nMed > 0 then begin
                self.InsertObject(nLow + 1, sName, TObject(nIndex));
                break;
              end
              else begin
                if nMed < 0 then begin
                  self.InsertObject(nLow, sName, TObject(nIndex));
                  break;
                end
                else begin
                  Result := False;
                  break;
                end;
              end;
            end;
          end else begin
            nCompareVal := CompareText(sName, self.Strings[nMed]);
            if nCompareVal > 0 then begin
              nLow := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            if nCompareVal < 0 then begin
              nHigh := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            Result := False;
            break;
          end;
        end;
      end;
    end;
  end;
end;

{ TQuickIDList }

procedure TQuickIDList.AddRecord(sAccount, sChrName: string; nIndex: Integer);
var
  QuickID                   : pTQuickID;
  ChrList                   : TList;
  nLow, nHigh, nMed, n1C, n20: Integer;
begin
  New(QuickID);
  QuickID.sAccount := sAccount;
  QuickID.sChrName := sChrName;
  QuickID.nIndex := nIndex;
  QuickID.nSelectID := 0;
  if Count = 0 then begin
    ChrList := TList.Create;
    ChrList.Add(QuickID);
    AddObject(sAccount, ChrList);
  end else begin
    if Count = 1 then begin
      nMed := CompareStr(sAccount, self.Strings[0]);
      if nMed > 0 then begin
        ChrList := TList.Create;
        ChrList.Add(QuickID);
        AddObject(sAccount, ChrList);
      end else begin
        if nMed < 0 then begin
          ChrList := TList.Create;
          ChrList.Add(QuickID);
          InsertObject(0, sAccount, ChrList);
        end else begin
          ChrList := TList(self.Objects[0]);
          ChrList.Add(QuickID);
        end;
      end;
    end else begin
      nLow := 0;
      nHigh := self.Count - 1;
      nMed := (nHigh - nLow) div 2 + nLow;
      while (True) do begin
        if (nHigh - nLow) = 1 then begin
          n20 := CompareStr(sAccount, self.Strings[nHigh]);
          if n20 > 0 then begin
            ChrList := TList.Create;
            ChrList.Add(QuickID);
            InsertObject(nHigh + 1, sAccount, ChrList);
            break;
          end else begin
            if CompareStr(sAccount, self.Strings[nHigh]) = 0 then begin
              ChrList := TList(self.Objects[nHigh]);
              ChrList.Add(QuickID);
              break;
            end else begin
              n20 := CompareStr(sAccount, self.Strings[nLow]);
              if n20 > 0 then begin
                ChrList := TList.Create;
                ChrList.Add(QuickID);
                InsertObject(nLow + 1, sAccount, ChrList);
                break;
              end else begin
                if n20 < 0 then begin
                  ChrList := TList.Create;
                  ChrList.Add(QuickID);
                  InsertObject(nLow, sAccount, ChrList);
                  break;
                end else begin
                  ChrList := TList(self.Objects[n20]);
                  ChrList.Add(QuickID);
                  break;
                end;
              end;
            end;
          end;
        end else begin
          n1C := CompareStr(sAccount, self.Strings[nMed]);
          if n1C > 0 then begin
            nLow := nMed;
            nMed := (nHigh - nLow) div 2 + nLow;
            Continue;
          end;
          if n1C < 0 then begin
            nHigh := nMed;
            nMed := (nHigh - nLow) div 2 + nLow;
            Continue;
          end;
          ChrList := TList(self.Objects[nMed]);
          ChrList.Add(QuickID);
          break;
        end;
      end;
    end;
  end;
end;

procedure TQuickIDList.DelRecord(nIndex: Integer; sChrName: string);
var
  QuickID                   : pTQuickID;
  ChrList                   : TList;
  i                         : Integer;
begin
  if (self.Count - 1) < nIndex then
    Exit;
  ChrList := TList(self.Objects[nIndex]);
  for i := 0 to ChrList.Count - 1 do begin
    QuickID := ChrList.Items[i];
    if QuickID.sChrName = sChrName then begin
      Dispose(QuickID);
      ChrList.Delete(i);
      break;
    end;
  end;
  if ChrList.Count <= 0 then begin
    ChrList.Free;
    self.Delete(nIndex);
  end;

end;

function TQuickIDList.GetChrList(sAccount: string; var ChrNameList: TList): Integer;
var
  nHigh, nLow, nMed, n20, n24: Integer;
begin
  Result := -1;
  if self.Count = 0 then
    Exit;
  if self.Count = 1 then begin
    if CompareStr(sAccount, self.Strings[0]) = 0 then begin
      ChrNameList := TList(self.Objects[0]);
      Result := 0;
    end;
  end else begin
    nLow := 0;
    nHigh := self.Count - 1;
    nMed := (nHigh - nLow) div 2 + nLow;
    n24 := -1;
    while (True) do begin
      if (nHigh - nLow) = 1 then begin
        if CompareStr(sAccount, self.Strings[nHigh]) = 0 then
          n24 := nHigh;
        if CompareStr(sAccount, self.Strings[nLow]) = 0 then
          n24 := nLow;
        break;
      end else begin
        n20 := CompareStr(sAccount, self.Strings[nMed]);
        if n20 > 0 then begin
          nLow := nMed;
          nMed := (nHigh - nLow) div 2 + nLow;
          Continue;
        end;
        if n20 < 0 then begin
          nHigh := nMed;
          nMed := (nHigh - nLow) div 2 + nLow;
          Continue;
        end;
        n24 := nMed;
        break;
      end;
    end;
    if n24 <> -1 then
      ChrNameList := TList(self.Objects[n24]);
    Result := n24;
  end;
end;

end.
