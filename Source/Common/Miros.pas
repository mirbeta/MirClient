unit Miros;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,winsock,urlmon,shellapi,Registry,iniFiles,TeeProcs, TeEngine, Chart,
  ExtCtrls, StrUtils,ComCtrls,DB,Buttons, ShlObj, ComObj, ActiveX,WinInet,
  Richedit,mmsystem;

type
 TCPUIDResult = packed record
   EAX: DWord;
   EBX: DWord;
   ECX: DWord;
   EDX: DWord;
 end;

 TMirosFindRootOnProgress = procedure (sCurRoot: string) of Object;

Function IPAddrToName(IPAddr : String): String;//通过IP地址获取主机名
Function Doloadfiles(Source,Dest:string):Boolean;//网络下载文件
Function GetSysDir:String;//获取系统目录
Function Eencode(str:string;code:int64):string;
Function Edecode(str:string;code:int64):string;
Function GetLocalName:String; //取本机名
Function GetLocalIP:String; //获取本机IP
Function GetTempDirectory:string;//获取临时文件目录
Function GetHzPy(const AHzStr: string): string;   //取汉字的拼音
Function getChineseCharcount(Const s:String):Integer; //检测字符中汉字数量
Function StrToHex(AStr: string): string; //汉字转16进制
Function HexToStr(AStr: string): string; //十六进制转化成字符
Function TransChar(AChar: Char): Integer;//？？？？？
Function IsBDEInstalled: boolean;   //判断BDE是否安装过。
Function DNSTOIP(DNSName: String): String; //DNS名转IP
Function ToBigRMB(RMB: string): string;//人民币大写
Function GetHardID:string;      //简单获取硬盘序列号
Function deldir(Dir: string): Boolean;//删除目录
Function CopyDir(const sDirName, sToDirName: string): Boolean;//拷贝目录的函数：CopyDir
Function GetfixTime(const Filename: string): string;  //取文件修改时间
Function GetFileSize(FileName: string): Longint; //获取文件大小
Function GetDirFilescount(Dir: string): Integer; //取目录内文件数量
Function StrCheck(str:string):boolean;//检查字符中是否含有非法字符
Function Internetconnected:boolean;//检查计算机是否联网
function DownloadFile(Source, Dest: string): Boolean;
Function Getlinemodle:byte;//取得上网类型 0没有连网 1无路由上网 2局域网上网 9忙碌中...
//Function CheckNet(IpAddr: string): Boolean;//检测是否能连接到主机
Function contrasttime(atime,oldtime: string):boolean; //对比时间
Function IsAlreadyaddstart(title:string):boolean;//是否已经加入到系统启动项
function GetFileIcon(const FileName:String;ASmall:Boolean):HIcon; //取文件的图标
function GetClassIcon(const AClass: string; ASmall: Boolean): HIcon;   //按后缀取图标
function Strfilter(str:string):string;//过滤字符串中的非法字符
function GetHardNumber: PAnsiChar;
function GetCpuBuilder : PAnsiChar;
function GetCpuIdA : integer;
function GetCPUID(EAX: DWord): TCPUIDResult;
function GetCPUID_Str(EAX: DWord): PAnsiChar;
function SoundInstall:Boolean;   //声卡是否安装
function TickCountToStr(dwTickCount	:	Cardinal):String;
function GetLocalUserName:string;//获取系统当前登陆用户名
function IsIPAddr(aIpAddr : string):Boolean;//IP是否正确
function AssembleString(Strs: array of String): string;
function FillNumStr(value, nLength: Integer; FillChr: Char = #0):string;
function GetIconIndex(const AFile: string; Attrs: DWORD): integer;              //取得图标的索引
function GetRealFileName(sFileName: string):string;
function DeleteFileExt(sFileName: string):string;
function IsFirstRun(sRoot,Key: string):Boolean;//是否第一次运行
function GetSCopyEnd(const SourceString: string; CopyTo: Integer): Integer;
//标准截取字符串如果是单双字节混合的字符串则保证截取的双字节字符可用
function GetTextCount(const Canvas: TCanvas; const nWidth: integer): Integer;
//获得画指定长度的字符串的长度
function IsTheseWordsIn(const CharArr: array of Char; const CheckStr: string):Boolean;
//HexToInt
function HexToInt(const S: string): DWORD;

//过程**********************************************************************
procedure RunFile(const FName: string; Handle: THandle = 0;const Param: string = '');   {测试通过}{* 运行一个文件}
procedure DeleteDir(SourcePath: String);   //删除目录（包括子文件夹以及具有只读属性的文件
procedure DelSelf;//运行完后删除自己
procedure Addicotoiebar(defaulthotico, defualtico, defaultrun, defaulthint, defaultcaption, defualtbartxt: string); //添加到IE工具拦
procedure SetCur(source:string;restore:boolean); //设置光标
Procedure Addlnk(info,cp:PChar);//创建桌面快捷方式
Procedure Mtest(H:THandle;s:string);
Procedure AddToStart(title,filename:string;B:boolean);
procedure AddRich(IO:TRichEdit;Str:PAnsiChar;FontClr: TColor = clBlack; GroundClr:Tcolor = clWhite;FontName:Tfontname = '宋体';FontSize:Byte = 12;Udline:Boolean = false);//向richedit加彩色背景文字;
//procedure AddRich(aRichEdit:TRichEdit; Text:PAnsiChar; Font : TFont);
procedure AddRichA(RichEdit : TRichEdit; Text : PAnsiChar; aFont : TFont; BackColor : TColor; UnderLine : Boolean);
procedure GetDeskMap(bmp:TBitmap);//截取屏幕
procedure GetDeskAriaMap(bmp : TBitmap; Rect : TRect);//获取屏幕区域
procedure ReBootWindows;
procedure DoNothing;
procedure FindAllDir(Path: string; DestResult : TStrings);overload;
procedure FindAllDir(Path: string; lpOnProgressEvent: TMirosFindRootOnProgress);Overload;
procedure FindAllFile(Path, DefExt: string; DestResult : TStrings ; boChild : Boolean = true);
procedure EnumDrivers(lpOnProgressEvent: TMirosFindRootOnProgress);
function  ConvertToShortFileName(Canvas: TCanvas; Source: string; WantWidth: Integer): string;
procedure GetSystemImageList(imagelist:TImageList);                             //获取系统图标到ImageList
procedure RegFileType(DefExt,DefFileType,DefFileInfo,ExeName:string;
                          IcoIndex:integer;
                          DoUpdate:boolean=false); //注册文件类型

//对比文件时间 时间A是否先于时间B
function  IsTimeBefor(Time1, Time2: TDateTime):Boolean;
procedure ShutDownWindows;


implementation

function IPAddrToName(IPAddr : String): String;  
  var
    SockAddrIn: TSockAddrIn;
    HostEnt: PHostEnt;
    WSAData: TWSAData;
  begin
    WSAStartup($101, WSAData);
    SockAddrIn.sin_addr.s_addr:= inet_addr(PAnsiChar(IPAddr));
    HostEnt:= gethostbyaddr(@SockAddrIn.sin_addr.S_addr, 4, AF_INET);
    if HostEnt<>nil then
    begin
      result:=StrPas(Hostent^.h_name)
    end
    else
    begin
      result:='';
    end;
  end;

Function Doloadfiles(Source,Dest:string):Boolean;//网络下载文件
begin
  try    
  Result := UrlDownloadToFile(nil, PChar(source), PChar(Dest), 0, nil) = 0;
  except
  Result := False; 
  end;
end;

//加密
function Eencode(str:string;code:int64):string;
var
i:word;
str2:string;
begin
for i:=1 to length(str) do
  begin
  str2:=str2+chr(ord(str[i])+code);
  end;
result:=str2;
end;
//解密
function Edecode(str:string;code:int64):string;
var
i:word;
str2:string;
begin
for i:=1 to length(str) do
  begin
  str2:=str2+chr(ord(str[i])-code);
  end;
result:=str2;
end;

function GetSysDir:String;//获取系统目录
var p:PChar;
begin
GetMem(P,255);
GetSystemDirectory(p,254);
Result := p;
Freemem(p);
end;

function GetLocalName:String; //取本机名
var
ComputerName: array[0..MAX_COMPUTERNAME_LENGTH+1] of char;
Size: Cardinal;
begin
result:='';
Size := MAX_COMPUTERNAME_LENGTH+1;
GetComputerName(ComputerName, Size);
Result:=StrPas(ComputerName);
end;

function GetLocalIP:String; //获取本机IP
var
  WSData: TWSAData;
  Buffer: string;// array[0..63] of Char;
  HostEnt: PHostEnt;
  PPInAddr: ^PInAddr;
  IPString: String;
begin
  IPString:='';
  try
    WSAStartUp($101, WSData);
    Buffer:=GetLocalName;
    HostEnt:=GetHostByName(PAnsiChar(Buffer));
    if Assigned(HostEnt) then
      begin
        PPInAddr  := @(PInAddr(HostEnt.H_Addr_List^));
        while Assigned(PPInAddr^) do
          begin
            IPString  := StrPas(INet_NToA(PPInAddr^^));
            Inc(PPInAddr);
          end;
      end;
    Result := IPString;
  finally
    try
      WSACleanUp;
    except
    end;
  end;
end; 

function GetTempDirectory:string;//获取临时文件目录
var
Len:integer;
begin
Len := GetTempPath(0,nil);
SetLength(Result,Len + 1);
GetTempPath(Len,@Result[1]);
end;

procedure RunFile(const FName: string; Handle: THandle;const Param: string);//运行一个文件
begin
  ShellExecute(Handle, nil, PChar(FName), PChar(Param), nil, SW_SHOWNORMAL);
end;

function GetFileSize(FileName: string): Longint; //获取文件大小
var
  SearchRec: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then
    Result := SearchRec.Size
  else
    Result := -1;
end;

function deldir(Dir: string): Boolean;
var
  sr: TSearchRec;
  fr: Integer;
begin
  if not DirectoryExists(Dir) then
  begin
    Result := True;
    Exit;
  end;
  fr := FindFirst(Dir+'\' + '*.*', faAnyFile, sr);
  try
    while fr = 0 do
    begin
      if (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        if sr.Attr and faDirectory = faDirectory then
          Result := deldir(Dir+'\' + sr.Name)
        else
          Result := DeleteFile(Dir+'\' + sr.Name);
        if not Result then
          Exit;
      end;
      fr := FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;
  Result := removeDir(Dir);
end;

function GetDirFilescount(Dir: string): Integer; //取目录内文件数量
var
  sr: TSearchRec;
  fr: Integer;
begin
  Result := 0;
  fr := FindFirst(Dir+'\' + '*.*', faAnyFile, sr);
  while fr = 0 do
  begin
    if (sr.Name <> '.') and (sr.Name <> '..') then
      Inc(Result);
    fr := FindNext(sr);
  end;
  FindClose(sr);
end;


function GetHzPy(const AHzStr: string): string;   //取汉字的拼音
const
  ChinaCode: array[0..25, 0..1] of Integer = ((1601, 1636), (1637, 1832), (1833, 2077),
    (2078, 2273), (2274, 2301), (2302, 2432), (2433, 2593), (2594, 2786), (9999, 0000),
    (2787, 3105), (3106, 3211), (3212, 3471), (3472, 3634), (3635, 3722), (3723, 3729),
    (3730, 3857), (3858, 4026), (4027, 4085), (4086, 4389), (4390, 4557), (9999, 0000),
    (9999, 0000), (4558, 4683), (4684, 4924), (4925, 5248), (5249, 5589));
var
  i, j, HzOrd: Integer;
begin
  Result:='';
  i := 1;
  while i <= Length(AHzStr) do
  begin
    if (AHzStr[i] >= #160) and (AHzStr[i + 1] >= #160) then
    begin
      HzOrd := (Ord(AHzStr[i]) - 160) * 100 + Ord(AHzStr[i + 1]) - 160;
      for j := 0 to 25 do
      begin
        if (HzOrd >= ChinaCode[j][0]) and (HzOrd <= ChinaCode[j][1]) then
        begin
          Result := Result + Char(Byte('A') + j);
          Break;
        end;
      end;
      Inc(i);
    end else Result := Result + AHzStr[i];
    Inc(i);
  end;
end;



function getChineseCharcount(Const s:String):Integer;//检测字符中汉字数量
var
   SW:WideString;
   C:String;
   i, WCount:Integer;
begin
   SW:=s;
   WCount:=0;
   For i:=1 to Length(SW) do
   begin
      c:=SW[i];
      if Length(c)>1 then
         Inc(WCount);
   end;
   Result:=WCount;
end;


function StrToHex(AStr: string): string; //汉字转16进制
var
   I : Integer;
//   Tmp: string;
   begin
      Result := '';
      For I := 1 to Length(AStr) do
      begin
         Result := Result + Format('%2x', [Byte(AStr[I])]);
      end;
      I := Pos(' ', Result);
      While I <> 0 do
      begin
         Result[I] := '0';
         I := Pos(' ', Result);
      end;
end;


function HexToStr(AStr: string): string; //十六进制转化成字符
var
   I : Integer;
   CharValue: Word;
   begin
   Result := '';
   for I := 1 to Trunc(Length(Astr)/2) do
   begin
      Result := Result + ' ';
      CharValue := TransChar(AStr[2*I-1])*16 + TransChar(AStr[2*I]);
      Result[I] := Char(CharValue);
   end;
end;

function TransChar(AChar: Char): Integer;
begin
   if AChar in ['0'..'9'] then
      Result := Ord(AChar) - Ord('0')
   else
      Result := 10 + Ord(AChar) - Ord('A');
end;



function IsBDEInstalled: boolean;   //判断BDE是否安装过。
var
  reg:TRegistry;
  s:string;
begin
   s:='';
   reg:=Tregistry.Create;
   reg.RootKey := HKEY_LOCAL_MACHINE;
   reg.OpenKey('SOFTWARE\Borland\Database Engine', False);
 try
    S:=reg.ReadString('CONFIGFILE01');
   //BDE installed
 finally
   if S<>'' then result:=True else result:=False;
   reg.CloseKey;
 end;
end;


procedure DeleteDir(SourcePath: String);   //删除目录（包括子文件夹以及具有只读属性的文件
var
  sr: TSearchRec;
begin
 // Screen.Cursor:=crHourGlass;
  SourcePath:=IncludeTrailingPathDelimiter(SourcePath);
  if FindFirst(SourcePath + '*.*', faAnyFile , sr) = 0 then
  begin
    repeat
      { 如果是只读或隐藏文件则先修改其属性为一般文件才能删除 }
      if ((SR.Attr and SysUtils.faReadOnly)<>0) or ((SR.Attr and faHidden)<> 0) then SetFileAttributes(PChar(SourcePath + sr.Name),FILE_ATTRIBUTE_NORMAL);
      if (sr.Attr = faDirectory + SysUtils.faReadOnly) or (sr.Attr = faDirectory + faHidden) or ((sr.Attr = faDirectory) and (sr.Name <> '.') and (sr.Name <> '..')) then
        begin
          { 有下级子目录也一并删除 }
          System.ChDir(sr.Name);
          DeleteDir(SourcePath + sr.Name);
          System.ChDir('..');
        end;
        Windows.DeleteFile(PChar(SourcePath + sr.Name));
    until FindNext(sr) <> 0;
    Windows.FindClose(sr.FindHandle);
    RemoveDir(SourcePath);
  end;
 // Screen.Cursor:=crDefault;
end;

function DNSTOIP(DNSName: String): String; //DNS名转IP
type
  TaPInAddr = array [0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe :PHostEnt;
  pptr : PaPInAddr;
  GInitData : TWSADATA;
begin
  WSAStartup($101, GInitData);
  Result := '';
  phe :=GetHostByName(PAnsiChar(DNSName));
  pptr := PaPInAddr(Phe^.h_addr_list);
  result:=StrPas(inet_ntoa(pptr^[0]^));
  WSACleanup;
end;

procedure DelSelf;//运行完后删除自己
var
  BatFile:TextFile;
begin
  assignfile(BatFile,'DelTemp.bat');
  rewrite(BatFile);
  writeln(BatFile,':Rdel');
  writeln(BatFile,'del '+ExtractFileName(application.ExeName));
  writeln(BatFile,'if exist '+ExtractFileName(application.ExeName)+' goto Rdel');
  writeln(BatFile,'del DelTemp.bat');
  closeFile(BatFile);
  ShellExecute(0,nil,'DelTemp.bat',nil,nil,SW_HIDE);
end;

function ToBigRMB(RMB: string): string;//人民币大写
const
  BigNumber = '零壹贰叁肆伍陆柒捌玖';
  BigUnit   = '万仟佰拾亿仟佰拾万仟佰拾元';
var
  nLeft,nRigth,lTemp,rTemp,BigNumber1,BigUnit1:string;
  I:Integer;
  minus:boolean;
begin
 try
  minus:=false;
  {取整数和小数部分}
  if StrToFloat(rmb)<0 then
    begin
      RMB:=FloatToStrF(ABS(StrToFloat(RMB)),fffixed,20,2);
      minus:=true;
    end
  else
      RMB:=FloatToStrF(ABS(StrToFloat(RMB)),fffixed,20,2);

  nLeft:=copy(RMB, 1, Pos('.', RMB) - 1);
  nRigth:=copy(RMB, Pos('.', RMB) + 1, 2);  {转换整数部分}
  for I:=1 to Length(nLeft) do
   begin
     BigNumber1:=copy(BigNumber, StrToInt(nLeft[I]) * 2 + 1, 2);
     BigUnit1:=copy(BigUnit, (Trunc(Length(BigUnit) / 2) - Length(nleft) + I - 1) * 2 + 1, 2);
     if (BigNumber1='零') and ((copy(lTemp, Length(lTemp)- 1, 2))='零') then
       lTemp:=copy(lTemp, 1, Length(lTemp) - 2);
     if (BigNumber1='零') and ((BigUnit1='亿') or (BigUnit1='万') or (BigUnit1='元')) then
       begin
         BigNumber1:=BigUnit1;
         if BigUnit1<>'元' then
            BigUnit1:='零'
         else
            BigUnit1:='';
       end;
     if (BigNumber1='零') and (BigUnit1<>'亿') and (BigUnit1<>'万') and (BigUnit1<>'元') then BigUnit1:='';
     lTemp:=lTemp + BigNumber1 + BigUnit1;
   end;
   if trim(ltemp)='元' then ltemp:='零'+ltemp;

   if Pos('亿万', lTemp)<>0 then Delete(lTemp, Pos('亿万', lTemp) + 2, 2);  {转换小数部分}

   if (trim(copy(ltemp,length(ltemp)-3,2))<>'')and(pos(copy(ltemp,length(ltemp)-3,2),bigunit)>0)and(StrToInt(nRigth[1])<>0 or StrToInt(nRigth[2])) then ltemp:=ltemp+'零';

   if (trim(ltemp)='零元')and(StrToInt(nRigth[1])<>0 or StrToInt(nRigth[2])) then ltemp:='';

   if minus then ltemp:='(负)'+ltemp;

   if StrToInt(nRigth[1])<>0 then rTemp:=copy(BigNumber, StrToInt(nRigth[1]) * 2 + 1, 2) + '角';

   if StrToInt(nRigth[2])<>0 then
     begin
       if (StrToInt(nRigth[1])=0)and((rightstr(ltemp,2)<>'零')and(trim(rightstr(ltemp,2))<>'')) then rTemp:='零';
       rTemp:=rTemp + copy(BigNumber, StrToInt(nRigth[2]) * 2 + 1, 2) + '分';
       Result:=lTemp + rTemp;
     end
   else
     Result:=lTemp + rTemp + '整';
  except
     Result:='非法数据';
  end;
end;


procedure Addicotoiebar(defaulthotico, defualtico, defaultrun, defaulthint, defaultcaption, defualtbartxt: string); //添加到IE工具拦
var 
rg:Tregistry;
begin
   rg:=Tregistry.create;
   rg.rootkey:=HKEY_LOCAL_MACHINE;
   rg.openkey('SOFTWARE\MICROSOFT\INTERNET EXPLORER\EXTENSIONS\{2713E8D2-850A-101B-AFC0-4210102A8DA7}',true);
   rg.writestring(defaultcaption,defualtbartxt);
   rg.writestring('CLSID','{1FBA04EE-3024-11D2-8F1F-0000F87ABD16}');
   rg.writestring('DEFAULT VISIBLE','YES');
   rg.writestring('ButtonText',defaulthint);
   rg.writestring('EXEC',defaultrun);
   rg.writestring('ICON',defaulthotico);
   rg.writestring('HOTICON',defualtico);
   rg.closekey;
   rg.free; 
end;


procedure SetCur(source:string;restore:boolean); //设置光标
var 
h: HCURSOR;
begin
  if restore=false then begin
    h:=LoadCursorFromFile(PChar(source));//从指定位置导入光标
    SetSystemCursor(h,ocr_normal);
  end
  else begin
    SystemParametersinfo(SPI_SETCURSORS,0,nil,SPIF_SENDCHANGE);
  end;
end;

Function GetHardID:string;      //简单获取硬盘序列号
var
dw,dwTemp1,dwTemp2:DWord;
p1,p2:array[0..30] of char;
begin
GetVolumeInformation(PChar('c:\'),p1,20,@dw,dwTemp1,dwTemp2,p2,20);
result:=inttohex(dw,18);//系列号
end;

function DoCopyDir(const sDirName, sToDirName: string): Boolean;
var
  hFindFile: Cardinal;
  //hFindFile: TSearchRec;
  t, tfile: string;
  sCurDir: string[255];
  FindFileData: WIN32_FIND_DATA;
begin
  Result := false;
  //先保存当前目录
  sCurDir := GetCurrentDir;
  ChDir(sDirName);
  hFindFile := FindFirstFile('*.*', FindFileData);
  if hFindFile <> INVALID_HANDLE_VALUE then
  begin
    if not DirectoryExists(sToDirName) then ForceDirectories(sToDirName);
    repeat
      tfile := FindFileData.cFileName;
      if (tfile = '.') or (tfile = '..') then Continue;
      if FindFileData.dwFileAttributes = FILE_ATTRIBUTE_DIRECTORY then
      begin
        t := sToDirName + '\' + tfile;
        if not DirectoryExists(t) then ForceDirectories(t);
        if sDirName[Length(sDirName)] <> '\' then
          DoCopyDir(sDirName + '\' + tfile, t)
        else
          DoCopyDir(sDirName + tfile, sToDirName + tfile);
      end
      else begin
        t := sToDirName + '\' + tFile;
        CopyFile(PChar(tfile), PChar(t), False);
      end;
    until FindNextFile(hFindFile, FindFileData) = false;
    //FindClose(hFindFile);
  end
  else begin
    ChDir(sCurDir);
    exit;
  end;
  //回到原来的目录下
  ChDir(sCurDir);
  Result := true;
end;


function CopyDir(const sDirName, sToDirName: string): Boolean;//拷贝目录的函数：CopyDir
begin
  Result := false;
  if Length(sDirName) <= 0 then exit;
  //拷贝...
  Result := DoCopyDir(sDirName, sToDirName);
end;

function CovFileDate(Fd: _FileTime): TDateTime;
{ 转换文件的时间格式 }
var
  Tct:_SystemTime;
  Temp:_FileTime;
begin
  FileTimeToLocalFileTime(Fd, Temp);
  FileTimeToSystemTime(Temp, Tct);
  CovFileDate := SystemTimeToDateTime(Tct);
end;

function GetfixTime(const Filename: string): string;  //取文件修改时间
{ 获取文件时间，Tf表示目标文件路径和名称 }
const
  Model = 'yyyy-mm-dd hh:mm:ss'; { 设定时间格式 }
var
  Tp: TSearchRec; { 申明Tp为一个查找记录 }
  T1, T2, T3: string;
begin
if fileexists(Filename) then
begin
  FindFirst(Filename, faAnyFile, Tp); { 查找目标文件 }
  T1 := FormatDateTime(Model, CovFileDate(Tp.FindData.ftCreationTime));
  { 返回文件的创建时间 }
  T2 := FormatDateTime(Model, CovFileDate(Tp.FindData.ftLastWriteTime));
  { 返回文件的修改时间 }
  T3 := FormatDateTime(Model, Now);
  Result := t2;
  { 返回文件的当前访问时间 }
  FindClose(Tp);
end
else
  Result := '';
//Result:='0000-00-00 0:0:0';
end;

function Strfilter(str:string):string;
begin
 str:=stringreplace(str,'~','',[rfreplaceall]);
 str:=stringreplace(str,'!','',[rfreplaceall]);
 str:=stringreplace(str,'@','',[rfreplaceall]);
 str:=stringreplace(str,'#','',[rfreplaceall]);
 str:=stringreplace(str,'$','',[rfreplaceall]);
 str:=stringreplace(str,'%','',[rfreplaceall]);
 str:=stringreplace(str,'^','',[rfreplaceall]);
 str:=stringreplace(str,'&','',[rfreplaceall]);
 str:=stringreplace(str,'*','',[rfreplaceall]);
// str:=stringreplace(str,'(','',[rfreplaceall]);
 str:=stringreplace(str,'~','',[rfreplaceall]);
 str:=stringreplace(str,'.','',[rfreplaceall]);
 str:=stringreplace(str,'/','',[rfreplaceall]);
 str:=stringreplace(str,'\','',[rfreplaceall]);
 str:=stringreplace(str,':','',[rfreplaceall]);
 str:=stringreplace(str,'"','',[rfreplaceall]);
 str:=stringreplace(str,'?','',[rfreplaceall]);
 str:=stringreplace(str,'<','',[rfreplaceall]);
 str:=stringreplace(str,'>','',[rfreplaceall]);
 str:=stringreplace(str,'|','',[rfreplaceall]); 
 //str:=stringreplace(str,' ','',[rfreplaceall]);
 result := str;
end;

Function StrCheck(str:string):boolean;
begin
if (pos(' ',str)>0) or (pos('~',str)>0) or  (pos('!',str)>0) or  (pos('@',str)>0) or  (pos('#',str)>0) or  (pos('%',str)>0) or  (pos('^',str)>0)or  (pos('&',str)>0) or  (pos('*',str)>0) or  (pos('-',str)>0) or  (pos('=',str)>0) or  (pos('+',str)>0) or  (pos('\',str)>0) or  (pos('/',str)>0) or  (pos('.',str)>0)
then result:=false else result:=true;
end;

Function internetconnected:boolean;
var
  outflag:integer;
begin
  //用该函数来检测，要在uses里加上WinINet引用WinINet单元
  if InternetGetConnectedState(@outflag,0)
then  Result:=True else Result:=False;
end;

function DownloadFile(Source, Dest: string): Boolean;
begin
try
Result := UrlDownloadToFile(nil, PChar(source), PChar(Dest), 0, nil) = 0;
except
Result := False;
end;
end;

Function Getlinemodle:byte;
var
  outflag:integer;
  bRet:Boolean;
begin
  Result := 255;
  bRet:=InternetGetConnectedState(@outflag,0);
  if not bRet then
  begin
    result:=0;
  end
  else
  begin
    if (outflag and INTERNET_CONNECTION_MODEM)<>0 then
    begin
      result:=1;
      //ShowMessage('Modem上网');
    end
    else if (outflag and INTERNET_CONNECTION_LAN)<>0 then
    begin
      result:=2;
      //ShowMessage('局域网上网');
    end
    else if (outflag and INTERNET_CONNECTION_MODEM_BUSY)<>0 then
    begin
      result:=9;
      //ShowMessage('忙碌中...');
    end;
  end;
end;


procedure AddRich(IO:TRichEdit;Str:PAnsiChar;FontClr: TColor = clBlack; GroundClr:Tcolor = clWhite;FontName:Tfontname = '宋体';FontSize:Byte = 12;Udline:Boolean = false);
var  Fmt :TCharFormat2;
begin
try
  IO.SelStart := Length(IO.Text);
  IO.SelLength:= 0;
  with IO.SelAttributes do
  begin
     color:= Fontclr;
     name:= FontName;
     height:= FontSize;
    if Udline then
      style:=[fsUnderline];
  end;
    Fmt.cbSize := SizeOf(Fmt);//这里放传递的结构大小，系统通过这个知道传递的是CharFormat还是CharFormat2
    Fmt.dwMask := CFM_COLOR or CFM_BACKCOLOR;//告诉系统只有字体颜色和背景颜色两个字段的值有效
    Fmt.crTextColor := FontClr;//设置字体颜色
    Fmt.crBackColor := GroundClr;//设置字体背景色
    IO.Perform(EM_SETCHARFORMAT,SCF_SELECTION,integer(@Fmt));//发EM_SETCHARFORMAT消息给RichEdit
    //其中SCF_SELECTION表示该设置只对选择的文字有效,具体用法参考win32 SDK HELP
    IO.Lines.Add(str);// 插入一行新文字
except
end;  
end;

Procedure Addlnk(info,cp:PChar);//创建桌面快捷方式
var
  ShLink: IShellLink;
  PFile: IPersistFile;
  FileName: string;
  WFileName: WideString;
  Reg: TRegIniFile;
  AnObj: IUnknown;
begin
  AnObj := CreateComObject(CLSID_ShellLink); //快捷方式的初始化
  ShLink := AnObj as IShellLink;
   PFile := AnObj as IPersistFile;
  FileName := ParamStr(0);
  ShLink.SetPath(PChar(FileName));
  ShLink.SetWorkingDirectory(PChar(ExtractFilePath(FileName)));
   ShLink.SetDescription(info);
  Reg := TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\Explorer');
  WFileName := Reg.ReadString('Shell Folders', 'Desktop', '') + '\' + Cp + '.lnk';
  Reg.Free;
  PFile.Save(PWChar(WFileName), False);
  Reg := TRegIniFile.Create('Software\MicroSoft\Windows\CurrentVersion\Explorer');
  WFileName := Reg.ReadString('Shell Folders', 'Start Menu', '') + '\' + Cp + '.lnk';
  Reg.Free;
  PFile.Save(PWChar(WFileName), False);
end;
//wangjimima918355746,zhaohuimima918355746 iD:533001198812264321

Procedure MTest(H:THandle;S:string);
begin
  messagebox(H,PChar(s),'Mtest',mb_ok+mb_iconinformation);
end;

Procedure AddToStart(title,filename:string;B:boolean);          //随系统启动程序
Var
reg:TRegistry;
begin
if b then
begin
    reg:=Tregistry.Create;
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('Software\MicroSoft\Windows\CurrentVersion\run', False);
    reg.writestring(title,filename);// OpenKey('Software\MicroSoft\Windows\CurrentVersion\run', False);
    reg.CloseKey;
    reg.free;
end
else begin
    reg:=Tregistry.Create;
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('Software\MicroSoft\Windows\CurrentVersion\run', False);
    reg.writestring(title,'');
    //reg.deletekey(title);
    reg.CloseKey;
    reg.free;
end;
end;

Function IsAlreadyaddstart(title:string):boolean;
Var
reg:Tregistry;
begin
  reg:=Tregistry.Create;
  reg.rootkey:= HKEY_LOCAL_MACHINE;
  reg.OpenKey('Software\MicroSoft\Windows\CurrentVersion\run', False);
  if reg.ReadString(title)='' then result:=False else result:=True;
  reg.CloseKey;
  reg.free;
end;

Function contrasttime(atime,oldtime: string):boolean;
Var
n,o:integer;
begin
         //2005-12-28只对比到天
  n:=StrToInt(stringreplace(copy(atime,1,10),'-','',[rfreplaceall]));
  o:=StrToInt(StringReplace(Copy(oldtime,1,10),'-','',[RfReplaceall]));
  if n<1 then n:=1;
  if n>o then
    result:=True
  else result:=False  
end;

function GetFileIcon(const FileName:String;ASmall:Boolean):HIcon; //取文件的图标
var
  FileInfo: TSHFileInfo;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  if ASmall then
  SHGetFileInfo(PChar(FileName),0,FileInfo,sizeof(FileInfo),SHGFI_ICON+SHGFI_SMALLICON)
  else
  SHGetFileInfo(PChar(FileName),0,FileInfo,sizeof(FileInfo),SHGFI_ICON+SHGFI_LARGEICON);
Result := FileInfo.HIcon;
//IMage1.Pictrue.Icon.Handle:=GetFileIcon(...);
end;


function GetClassIcon(const AClass: string; ASmall: Boolean): HIcon;   //按后缀取图标
var
  Info : TSHFileInfo;
  Flags : Cardinal;
begin
  if ASmall then
    Flags := SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES
  else
    Flags := SHGFI_ICON or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES;
  SHGetFileInfo(PChar(AClass), FILE_ATTRIBUTE_NORMAL, Info, SizeOf(TSHFileInfo), Flags);
  Result := Info.hIcon;
//IMage1.Pictrue.Icon.Handle:=GetClassIcon(...);
end;


//获得硬盘序列号
function GetHardNumber: PAnsiChar;
const IDENTIFY_BUFFER_SIZE = 512;
type
   TIDERegs = packed record
     bFeaturesReg: BYTE; // Used for specifying SMART "commands".
     bSectorCountReg: BYTE; // IDE sector count register
     bSectorNumberReg: BYTE; // IDE sector number register
     bCylLowReg: BYTE; // IDE low order cylinder value
     bCylHighReg: BYTE; // IDE high order cylinder value
     bDriveHeadReg: BYTE; // IDE drive/head register
     bCommandReg: BYTE; // Actual IDE command.
     bReserved: BYTE; // reserved for future use. Must be zero.
  end;
  TSendCmdInParams = packed record
    // Buffer size in bytes
    cBufferSize: DWORD;
    // Structure with drive register values.
    irDriveRegs: TIDERegs;
    // Physical drive number to send command to (0,1,2,3).
    bDriveNumber: BYTE;
    bReserved: array[0..2] of Byte;
    dwReserved: array[0..3] of DWORD;
    bBuffer: array[0..0] of Byte; // Input buffer.
  end;
  TIdSector = packed record
    wGenConfig: Word;
    wNumCyls: Word;
    wReserved: Word;
    wNumHeads: Word;
    wBytesPerTrack: Word;
    wBytesPerSector: Word;
    wSectorsPerTrack: Word;
    wVendorUnique: array[0..2] of Word;
    sSerialNumber: array[0..19] of CHAR;
    wBufferType: Word;
    wBufferSize: Word;
    wECCSize: Word;
    sFirmwareRev: array[0..7] of Char;
    sModelNumber: array[0..39] of Char;
    wMoreVendorUnique: Word;
    wDoubleWordIO: Word;
    wCapabilities: Word;
    wReserved1: Word;
    wPIOTiming: Word;
    wDMATiming: Word;
    wBS: Word;
    wNumCurrentCyls: Word;
    wNumCurrentHeads: Word;
    wNumCurrentSectorsPerTrack: Word;
    ulCurrentSectorCapacity: DWORD;
    wMultSectorStuff: Word;
    ulTotalAddressableSectors: DWORD;
    wSingleWordDMA: Word;
    wMultiWordDMA: Word;
    bReserved: array[0..127] of BYTE;
  end;
  PIdSector = ^TIdSector;
  TDriverStatus = packed record
    // 驱动器返回的错误代码，无错则返回0
    bDriverError: Byte;
    // IDE出错寄存器的内容，只有当bDriverError 为 SMART_IDE_ERROR 时有效
    bIDEStatus: Byte;
    bReserved: array[0..1] of Byte;
    dwReserved: array[0..1] of DWORD;
  end;
  TSendCmdOutParams = packed record
    // bBuffer的大小
    cBufferSize: DWORD;
    // 驱动器状态
    DriverStatus: TDriverStatus;
    // 用于保存从驱动器读出的数据的缓冲区，实际长度由cBufferSize决定
    bBuffer: array[0..0] of BYTE;
  end;
var
  hDevice: Thandle;
  cbBytesReturned: DWORD;
  SCIP: TSendCmdInParams;
  aIdOutCmd: array[0..(SizeOf(TSendCmdOutParams) + IDENTIFY_BUFFER_SIZE - 1) - 1] of Byte;
  IdOutCmd: TSendCmdOutParams absolute aIdOutCmd;
procedure ChangeByteOrder(var Data; Size: Integer);
var
  ptr: PAnsiChar;
  i: Integer;
  c: AnsiChar;
begin
  ptr := @Data;
  for I := 0 to (Size shr 1) - 1 do begin
    c := ptr^;
    ptr^ := (ptr + 1)^;
    (ptr + 1)^ := c;
    Inc(ptr, 2);
  end;
end;
begin
Result := ''; // 如果出错则返回空串
if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then begin // Windows NT, Windows 2000
// 提示! 改变名称可适用于其它驱动器，如第二个驱动器： '\\.\PhysicalDrive1\'
hDevice := CreateFile('\\.\PhysicalDrive0', GENERIC_READ or GENERIC_WRITE,
FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
end else // Version Windows 95 OSR2, Windows 98
hDevice := CreateFile('\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0);
if hDevice = INVALID_HANDLE_VALUE then Exit;
try
FillChar(SCIP, SizeOf(TSendCmdInParams) - 1, #0);
FillChar(aIdOutCmd, SizeOf(aIdOutCmd), #0);
cbBytesReturned := 0;
// Set up data structures for IDENTIFY command.
with SCIP do begin
cBufferSize := IDENTIFY_BUFFER_SIZE;
with irDriveRegs do begin
bSectorCountReg := 1;
bSectorNumberReg := 1;
bDriveHeadReg := $A0;
bCommandReg := $EC;
end;
end;
if not DeviceIoControl(hDevice, $0007C088, @SCIP, SizeOf(TSendCmdInParams) - 1,
@aIdOutCmd, SizeOf(aIdOutCmd), cbBytesReturned, nil) then Exit;
finally
CloseHandle(hDevice);
end;
with PIdSector(@IdOutCmd.bBuffer)^ do begin
ChangeByteOrder(sSerialNumber, SizeOf(sSerialNumber));
(PAnsiChar(@sSerialNumber) + SizeOf(sSerialNumber))^ := #0;
Result := PAnsiChar(@sSerialNumber);
end;
end;

function GetCPUbuilder : PAnsiChar;
var  
   R:  array[0..19]  of  AnsiChar;
   CpuID:  Integer;
begin
   FillChar(R,  20,  0);
   asm
       mov  eax,  0
       db  0fh,  0a2h
       mov  dword  ptr  R[0],    ebx
       mov  dword  ptr  R[4],    edx
       mov  dword  ptr  R[8],    ecx
       mov  eax,  1
       db  0fh,  0a2h
       mov  CpuID,  edx
   end;
   result := R;
end;

function GetCPUIdA : integer;
var  
   R:  array[0..19]  of  AnsiChar;
   CpuID:  Integer;
begin
   FillChar(R,  20,  0);
   asm
       mov  eax,  0
       db  0fh,  0a2h
       mov  dword  ptr  R[0],    ebx
       mov  dword  ptr  R[4],    edx
       mov  dword  ptr  R[8],    ecx
       mov  eax,  1
       db  0fh,  0a2h
       mov  CpuID,  edx
   end;
   result := CPUId
end;

function GetCPUID(EAX: DWord): TCPUIDResult;
var
 rEAX, rEBX, rECX, rEDX: DWord;
begin
 asm
   push EAX
   push EBX
   push ECX
   push EDX
   mov EAX,EAX
   //******************************************************
   //cpuid指令，因为Delphi的汇编编译器没有内置该指令，
   //所以用该指令的机器语言代码$0F,$A2来实现
   //******************************************************
   db $0F,$A2
   mov rEAX,EAX
   mov rEBX,EBX
   mov rECX,ECX
   mov rEDX,EDX
   pop EDX
   pop ECX
   pop EBX
   pop EAX
 end;
 Result.EAX := rEAX;
 Result.EBX := rEBX;
 Result.ECX := rECX;
 Result.EDX := rEDX;
end;

function GetCPUID_Str(EAX: DWord): PAnsiChar;
var
 rEAX, rEBX, rECX, rEDX: DWord;
begin
 asm
   push EAX
   push EBX
   push ECX
   push EDX
   mov EAX,EAX
   //******************************************************
   //cpuid指令，因为Delphi的汇编编译器没有内置该指令，
   //所以用该指令的机器语言代码$0F,$A2来实现
   //******************************************************
   db $0F,$A2
   mov rEAX,EAX
   mov rEBX,EBX
   mov rECX,ECX
   mov rEDX,EDX
   pop EDX
   pop ECX
   pop EBX
   pop EAX
 end;
 result := PAnsiChar(Format('%d-%d-%d-%d',[rEAX,rEBX,rECX,rEDX]));
end;

procedure GetDeskMap(bmp:TBitmap);
var
  DeskTopDC: HDc;
  DeskTopCanvas: TCanvas;
  DeskTopRect: TRect;
begin
  DeskTopDC := GetWindowDC(GetDeskTopWindow);
  DeskTopCanvas := TCanvas.Create;
  DeskTopCanvas.Handle := DeskTopDC;
  DeskTopRect := Rect(0,0,Screen.Width,Screen.Height);
  with bmp do
  begin
    Width := Screen.Width;
    Height:= Screen.Height;
    PixelFormat := pfDevice;
  end;
  bmp.Canvas.CopyRect(DeskTopRect,DeskTopCanvas,DeskTopRect);
  DesktopCanvas.Free;
  ReleaseDC(GetDeskTopWindow,DeskTopDC);
end;

procedure GetDeskAriaMap(bmp : TBitmap; Rect : TRect);
var
  DeskTopDC: HDc;
  DeskTopCanvas: TCanvas;
  DeskTopRect: TRect;
begin
  DeskTopDC := GetWindowDC(GetDeskTopWindow);
  DeskTopCanvas := TCanvas.Create;
  DeskTopCanvas.Handle := DeskTopDC;
  DeskTopRect := Rect;
  with bmp do
  begin
    Width := Screen.Width;
    Height:= Screen.Height;
    PixelFormat := pfDevice;
  end;
  bmp.Canvas.CopyRect(DeskTopRect,DeskTopCanvas,DeskTopRect);
  DesktopCanvas.Free;
  ReleaseDC(GetDeskTopWindow,DeskTopDC);
end;

procedure AddRichA(RichEdit : TRichEdit; Text : PAnsiChar; aFont : TFont; BackColor : TColor; UnderLine : Boolean);
var  Fmt :TCharFormat2;
begin
	try
 		 with RichEdit.SelAttributes do
 		 begin
     		color:= aFont.Color;
     		name:= aFont.Name;
     		height:= aFont.Height;
   		 if UnderLine then
      		style:=[fsUnderline];
 		 end;
    Fmt.cbSize := SizeOf(Fmt);//这里放传递的结构大小，系统通过这个知道传递的是CharFormat还是CharFormat2
    Fmt.dwMask := CFM_COLOR or CFM_BACKCOLOR;//告诉系统只有字体颜色和背景颜色两个字段的值有效
    Fmt.crTextColor := aFont.Color;//设置字体颜色
    Fmt.crBackColor := BackColor;//设置字体背景色
    RichEdit.Perform(EM_SETCHARFORMAT,SCF_SELECTION,integer(@Fmt));//发EM_SETCHARFORMAT消息给RichEdit
    //其中SCF_SELECTION表示该设置只对选择的文字有效,具体用法参考win32 SDK HELP
    RichEdit.Lines.Add(Text);// 插入一行新文字
	except
	end;
end;

{判断是哪类操作系统，以确定关机方式}
function GetOperatingSystem: Boolean;
var  osVerInfo: TOSVersionInfo;
begin
  Result :=False;
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT:
        begin
          Result := True;
        end;
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          Result := False;
        end;
    end;
end;



{定时关机函数 ，各参数的意义如下：
Computer: 计算机名;Msg:显示的提示信息;
Time:时间延迟; Force:是否强制关机;
Reboot: 是否重启动}
function W2KShutDown(Computer: string; Msg: string;
  Time: Word; Force: Boolean; Reboot: Boolean): Boolean;
var
  rl: Cardinal;
  hToken: Cardinal;
  tkp: TOKEN_PRIVILEGES;
begin
  {获得用户关机特权，仅对Windows NT/2000/XP}
  OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, 
hToken);
  if LookupPrivilegeValue(nil, 'SeShutdownPrivilege', tkp.Privileges[0].Luid) 
then
  begin
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    tkp.PrivilegeCount := 1;
    AdjustTokenPrivileges(hToken, False, tkp, 0, nil, rl);
  end;
  Result := InitiateSystemShutdown(PChar(Computer), PChar(Msg), Time, Force, Reboot)
end;
{重新启动计算机jamesread,在win2000ADVServer测试通过}
procedure ReBootWindows;
begin
  W2KShutDown(GetLocalName,'shutdown',1,true,true);
end;

procedure ShutDownWindows;
begin
  W2KShutDown(GetLocalName,'shutdown',1,true,FALSE);
end;

function SoundInstall:Boolean;
begin
  Result := WaveOutGetNumDevs()>0;
end;

function TickCountToStr(dwTickCount	:	Cardinal):String;
const
  ASecTick          = 1000;
  AMinTick          = ASecTick * 60;
  AHourTick         = AMinTick * 60;
  ADaysTick         = AHourTick * 24;
var
  TickCount: Cardinal;
  nVal: Integer;
begin
  Result := '';
  TickCount := dwTickCount;

  nVal := TickCount div ADaysTick;
  if nVal > 0 then
  begin
    Result := Result + IntToStr(nVal) + '天';
    Dec(TickCount, nVal * ADaysTick);
  end;

  nVal := TickCount div AHourTick;
  if nVal > 0 then
  begin
    Result := Result + IntToStr(nVal) + '小时';
    Dec(TickCount, nVal * AHourTick);
  end;

  nVal := TickCount div AMinTick;
  if nVal > 0 then
  begin
    Result := Result + IntToStr(nVal) + '分';
    Dec(TickCount, nVal * AMinTick);
  end;

  nVal := TickCount div ASecTick;
  if nVal > 0 then
  begin
    Result := Result + IntToStr(nVal) + '秒';
  end;

  nVal := TickCount mod ASecTick;
  if nVal > 0 then
  begin
    Result := Result + IntToStr(nVal) + '毫秒';
  end;
end;


procedure DoNothing;
begin
 //
end;

procedure RegisterFileType(FileExt, Key, Typestring, DefaultIcon, ExeName: string);
var myreg: treginifile;
  ct: integer;
begin

  // 取文件的后缀
  ct := pos('.', FileExt);
  while ct > 0 do begin
    delete(FileExt, ct, 1);
    ct := pos('.', FileExt);
  end;
  if (FileExt = '') or (ExeName = '') then exit; //判断后缀及应用程序是否有效
  FileExt := '.' + FileExt;
  myreg := treginifile.create('');
  try
    myreg.rootkey := hkey_classes_root; // 根应该为HKEY_CLASSES_ROOT
    if key = '' then key := copy(FileExt, 2, maxint) + '_auto_file';
    // 如果没给出键值，则自动创建一个
    myreg.writestring(FileExt, '', key); // 写入描述的键值
    myreg.writestring(key, '', Typestring); // 写入描述
    if DefaultIcon <> '' then
      myreg.writestring(key + '\DefaultIcon', '', DefaultIcon);
    // 写入缺省图标
    myreg.writestring(key + '\shell\open\command', '', ExeName + ' "%1"');
    //写入相关联的应用程序
  finally
    myreg.free;
  end;
end;

function GetLocalUserName:string;
Var 
 lpBuffer :Array[1..64] of Char;
 nSize    :Cardinal;
 sUserName : String;
Begin
 //取登录用户名
 nSize := 64;
 GetUserName(@lpBuffer, nSize);
 sUserName := lpBuffer;
 sUserName := Copy(sUserName, 1, nSize-1);
 Result := sUserName;
end;

function IsIPAddr(aIpAddr : string):Boolean;
var
  P1, P2, P3, P4: string;
begin
  P1 := Copy(aIpAddr,1, pos('.',aIpAddr)-1);
  delete(aIpAddr,1,pos('.',aIpAddr));
  P2 := Copy(aIpAddr,1, pos('.',aIpAddr)-1);
  delete(aIpAddr,1,pos('.',aIpAddr));
  P3 := Copy(aIpAddr,1, pos('.',aIpAddr)-1);
  delete(aIpAddr,1,pos('.',aIpAddr));
  P4 :=  aIpAddr;
  Result := (Pos('.',P1) <= 0) and ((Pos('.',P2) <= 0)) and ((Pos('.',P4) <= 0))
            and ((Pos('.',P3) <= 0));
  if Result then
    Result := (P1 <> '') and (P2 <> '') and (P3 <> '') and (P4 <> '');
  if Result then
    Try
      Result := (StrToInt(P1) <=255) and (StrToInt(P1) >= 0);
      if Result then
        Result := (StrToInt(P2) <=255) and (StrToInt(P2) >= 0);
      if Result then
        Result := (StrToInt(P3) <=255) and (StrToInt(P3) >= 0);
      if Result then
        Result := (StrToInt(P4) <=255) and (StrToInt(P4) >= 0);
    except
      Result := False;
    end;
end;

procedure GetSystemImageList(imagelist:TImageList); 
var 
   SysIL: THandle; 
   SFI: TSHFileInfo; 
begin 
   // 取小图标，如果将SHGFI_SMALLICON替换成 
   //SHGFI_LARGEICON则表示取大图标 
   SysIL := SHGetFileInfo('', 0, SFI, SizeOf(SFI), 
       SHGFI_SYSICONINDEX or SHGFI_SMALLICON); 
   if SysIL <> 0 then begin 
       //将imagelist的图像列表句柄指向系统图像句柄 
       imagelist.Handle := SysIL; 
       //防止组件释放时释放图像句柄，很重要 
       imagelist.ShareImages := TRUE; 
   end; 
end; 

//第二步  取得要处理文件的图标索引
//取一个文件的图标索引 
function GetIconIndex(const AFile: string; Attrs: DWORD): integer; 
//Attrs可以为表示文件或路径FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_DIRECTORY 
var 
   SFI: TSHFileInfo;       
begin 
   SHGetFileInfo(PChar(AFile), Attrs, SFI, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES); 
   Result := SFI.iIcon;
end; 

function AssembleString(Strs: array of String): string;
var
  I: Integer;
begin
   Result := '';
  for I := 0 to High(Strs) do
   Result := Result + Strs[I];
end;

function FillNumStr(Value, nLength: Integer; FillChr: Char = #0):string;
begin
 Result := IntToStr(Value);
 while Length(Result) < nLength do
    Result := FillChr + Result
end;

function GetRealFileName(sFileName: string):string;
begin
  Result := '';
  while pos('/', sFileName) > 0  do
    Delete(sFileName, 1, pos('/', sFileName));
  while pos('\', sFileName) > 0  do
    Delete(sFileName, 1, pos('\', sFileName));
  while Pos('.',sFileName) > 0 do
   begin
     Result := Result + Copy(sFileName,1,pos('.',sFileName));
     delete(sFileName,1,Pos('.',sFileName));
   end;
  if Result = '' then
   Result := sFileName
   else if Result[Length(Result)] = '.' then Result := Copy(Result,1,Length(Result) -1);
end;

function DeleteFileExt(sFileName: string):string;
begin
  while Pos('.',sFileName) > 0 do
   begin
     Result := Result + Copy(sFileName,1,pos('.',sFileName));
     delete(sFileName,1,Pos('.',sFileName));
   end;
  if Result = '' then
   Result := sFileName
   else if Result[Length(Result)] = '.' then Result := Copy(Result,1,Length(Result) -1);
end;

function IsFirstRun(sRoot,Key: string):Boolean;
var
  Reg: TRegistry;
begin
  Reg:= TRegistry.Create;
  with Reg do
   begin
    RootKey := HKEY_LOCAL_MACHINE;
    Result := OpenKey(sRoot,true);
    if Result then
      Result := ReadString(Key) = '';
    if Result then WriteString(Key,'0'); 
    CloseKey;
   end;
   FreeAndNil(Reg);
end;

procedure RegFileType(DefExt,DefFileType,DefFileInfo,ExeName:string;
                          IcoIndex:integer;
                          DoUpdate:boolean=false);
var
  Reg: TRegistry;
begin
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_CLASSES_ROOT;
    Reg.OpenKey(DefExt, True);
    //写入自定义文件后缀
    Reg.WriteString('', DefFileType);
    Reg.CloseKey;
    //写入自定义的文件类型
    //格式为：HKEY_CLASSES_ROOT\cMyExt\(Default) = 'cMyFileType'

    //下面为该文件类型创建关联
    Reg.OpenKey(DefFileType, True);
    Reg.WriteString('', DefFileInfo);
    //写入文件类型的描述信息
    Reg.CloseKey;

    // 下面为自定义文件类型选择图标
    // 加入键格式为 HKEY_CLASSES_ROOT\cMyFileType\DefaultIcon
    //  \(Default) = 'Application Dir\Project1.exe,0'
    Reg.OpenKey(DefFileType + '\DefaultIcon', True);
    Reg.WriteString('', ExeName + ',' + IntToStr(IcoIndex));
    Reg.CloseKey;

    // 下面注册在资源管理器中打开文件的程序
    Reg.OpenKey(DefFileType + '\Shell\Open', True);
    Reg.WriteString('', '打开 (&O)');
    Reg.CloseKey;

    Reg.OpenKey(DefFileType + '\Shell\Edit', True);
    Reg.WriteString('', '使用'+ DefFileType+ '编辑 (&E)');
    Reg.CloseKey;

    Reg.OpenKey(DefFileType + '\Shell\Edit\Command', True);
    Reg.WriteString('', '"' + ExeName + '" "%1"');
    Reg.CloseKey;    
    
    //  格式：HKEY_CLASSES_ROOT\Project1.FileType\Shell\Open\Command
    //  (Default) = '"Application Dir\Project1.exe" "%1"'
    Reg.OpenKey(DefFileType + '\Shell\Open\Command', True);
    Reg.WriteString('', '"' + ExeName + '" "%1"');
    Reg.CloseKey;

    //最后，让资源管理器实现我们加入的文件类型，只需调用SHChangeNotify即可
    if DoUpdate then SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  finally
    Reg.Free;
  end;
end;

function GetSCopyEnd(const SourceString: string; CopyTo: Integer): Integer;
var
  HC, I: Integer;
begin
  if CopyTo >= Length(SourceString) then begin
   Result := CopyTo;
   Exit;
  end;
  HC := 0;
  for I := 1 to CopyTo  do
   begin
    if Ord(SourceString[I]) >= 127 then
     inc(HC);
   end;
  if (HC mod 2 = 1) then Result := CopyTo - 1
  else Result := CopyTo;
  {
   宋体 9  30  180
           66  400
           133 800
  }
end;

function GetTextCount(const Canvas: TCanvas; const nWidth: integer): Integer;
const TestChar = 'A';
var
  S : String;
begin
  with Canvas do
   begin
     while TextWidth(S) < nWidth do
       S := S + TestChar;
   end;
  Result := Length(S) - 2;
end;

function IsTheseWordsIn(const CharArr: array of Char; const CheckStr: string):Boolean;
var
  I : Integer;
begin
  for I := 0 to High(CharArr) do
   begin
     Result := pos(CharArr[I],CheckStr) > 0;
     if Result then
      Break;
   end;
end;

procedure FindAllDir(Path: string; DestResult : TStrings);
var
  Sr: TSearchRec;
begin
  if FindFirst(Path + '\*.*', faAnyfile, Sr) = 0 then
  if (Sr.Name <> '.' ) and (Sr.Name <> '..') then
   if (Sr.Attr and faDirectory)=faDirectory then
    begin
      DestResult.Add(path + '\' + Sr.Name);
      FindAllDir(Path + '\' + Sr.Name, DestResult);
    end;
   while FindNext(Sr) = 0 do
    begin
     if (Sr.Name <> '.' ) and (Sr.Name <> '..') then
     if (Sr.Attr and faDirectory)=faDirectory then
      begin
       DestResult.Add(path + '\' + Sr.Name);
       FindAllDir(Path + '\' + Sr.Name, DestResult);
      end;
      Application.ProcessMessages;
    end;
  FindClose(Sr);
end;

procedure FindAllDir(Path: string; lpOnProgressEvent: TMirosFindRootOnProgress);Overload;
var
  Sr: TSearchRec;
begin
  if FindFirst(Path + '\*.*', faAnyfile, Sr) = 0 then
  if (Sr.Name <> '.' ) and (Sr.Name <> '..') then
   if (Sr.Attr and faDirectory)=faDirectory then
    begin
      if Assigned(lpOnProgressEvent) then
        lpOnProgressEvent(Path + '\' + Sr.Name);
      FindAllDir(Path + '\' + Sr.Name, lpOnProgressEvent);
    end;
   while FindNext(Sr) = 0 do
    begin
     if (Sr.Name <> '.' ) and (Sr.Name <> '..') then
     if (Sr.Attr and faDirectory)=faDirectory then
      begin
      if Assigned(lpOnProgressEvent) then
        lpOnProgressEvent(Path + '\' + Sr.Name);
      FindAllDir(Path + '\' + Sr.Name, lpOnProgressEvent);
      end;
      Application.ProcessMessages;
    end;
  FindClose(Sr);
end;

procedure FindAllFile(Path, DefExt: string; DestResult : TStrings ; boChild : Boolean);
var
  Sr: TSearchRec;
  SL: TStrings;
  I : Integer;
begin
  if boChild then
  begin
    SL := TStringList.Create;
    SL.Add(Path);
    FindAllDir(Path, SL);

    for I := 0 to SL.Count - 1 do
    begin
       if FindFirst(SL[I] + '\' + DefExt, faAnyFile, Sr) = 0 then
       begin
        if (Sr.Name <> '..') and (Sr.Name <> '.') then
        DestResult.Add(SL[I] + '\' + Sr.Name);

       end;
      while FindNext(Sr) = 0 do
        begin
          if (Sr.Name <> '..') and (Sr.Name <> '.') then
          begin
            DestResult.Add(SL[I] + '\' + Sr.Name);
            Application.ProcessMessages;
          end;
        end;
    end;
    FreeAndNil(SL);
    FindClose(Sr);
  end else
  begin
    if FindFirst(Path + DefExt , faAnyFile ,Sr) = 0 then
    begin
      if (Sr.Attr and faArchive <> 0) and ( Sr.Attr and faDirectory = 0) then
      begin
        DestResult.Add(Path + Sr.Name);
      end;
      while FindNext(Sr) = 0 do
      begin
        if (Sr.Attr and faArchive <> 0) and ( Sr.Attr and faDirectory = 0) then
        begin
          DestResult.Add(Path + Sr.Name);
        end;
      end;

    end;

    FindClose(Sr);
    
  end;
end;

procedure EnumDrivers(lpOnProgressEvent: TMirosFindRootOnProgress);
var
  DriverBuff: array [0..MAX_PATH - 1] of Char;
  nCount, I: Integer;
begin
  nCount := GetLogicalDriveStrings(MAX_PATH, DriverBuff);
  for I := 0 to nCount div 4 do
   lpOnProgressEvent(DriverBuff[I * 4]);
end;

function ConvertToShortFileName(Canvas: TCanvas; Source: string; WantWidth:
  Integer): string;
var
  i, len: Integer;
  str: string;
  function CutHalfCode(astr: string): string;
  var
    Pos, len: Integer;
  begin
    Result := '';
    Pos := 1;
    len := Length(astr);
    while True do
      begin
        if Pos > len then
          break;
        if (astr[Pos] > #127) then
          begin
            if ((Pos + 1) <= len) and (astr[Pos + 1] > #127) then
              begin
                Result := Result + astr[Pos] + astr[Pos + 1];
                Inc(Pos);
              end;

          end
          else
            Result := Result + astr[Pos];
        Inc(Pos);
    end;
  end;
begin
    if Length(Source) > 3 then
      if Canvas.TextWidth(Source) > WantWidth then
        begin
          len := Length(Source);
          for i := 1 to len do
            begin
              str := Copy(Source, 1, (len - i));
              str := str + '...';
              if Canvas.TextWidth(str) < (WantWidth - 4) then
                begin
                  Result := CutHalfCode(str);
                  exit;
                end;
            end;
          Result := CutHalfCode(Copy(Source, 1, 2)) + '...';
          exit;
        end;
    Result := Source;
end;

function HexToInt(const S: string): DWORD;
const
  Convert: array[0..255] of Integer =
    (
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
     );
var
  I: Integer;
  v: Integer;
begin
  Result := 0;
  if Pointer(s) = nil then exit;
  for I := 1 to PInteger(Integer(s) - 4)^ do
  begin
    begin
      V := Convert[ord(s[i])];
      if V<0 then
      begin
        Result := 0;
        Exit;
      end;
      result := (result * 16) or V;
    end;
  end;
end;

function  IsTimeBefor(Time1, Time2: TDateTime):Boolean;
var
  Y, M, D, H, N, S, MS: Word;
  Y2, M2, D2, H2, N2, S2, MS2: Word;
begin
  Result := False;
  DeCodeDate(Time1, Y, M, D);
  DeCodeTime(Time1, H, N, S, MS);
  DeCodeDate(Time2, Y2, M2, D2);
  DeCodeTime(Time2,H2, N2, S2, MS2);

{  ShowMessage(Format('%d-%d-%d %d:%d:%d:%d', [Y, M, D, H, N, S, MS])
              + #13 +
              Format('%d-%d-%d %d:%d:%d:%d', [Y2, M2, D2, H2, N2, S2, MS2]));  }

  if Y2 > Y then
    begin
      Result := True;
      Exit;
    end;
  if Y2 < Y then Exit;
  if M2 > M then
    begin
      Result := True;
      Exit;
    end;
  if M2 < M then Exit;
  if D2 > D then
    begin
      Result := True;
      Exit;
    end;
  if D2 < D then Exit;
  if H2 > H then
    begin
      Result := True;
      Exit;
    end;
  if H2 < H then Exit;
  if N2 > N then
    begin
      Result := True;
      Exit;
    end;
  if N2 < N then Exit;
  if S2 > S then
    begin
      Result := True;
      Exit;
    end;
  if S2 < S then Exit;
  if MS2 > MS then
    begin
      Result := True;
      Exit;
    end;    
end;

end.

