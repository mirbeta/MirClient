{*******************************************************}
{               MiTeC Common Routines                   }
{                   CRC routines                        }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$I Compilers.inc}

unit MiTeC_CRC;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_Mappings;

function CRC16(const AText: string): Word;
function CRC16Buffer(AData: PByte; ASize: int64): Word;

function CRC32Stream(AStream: TStream; ABufferSize: Cardinal = 1024): Cardinal;
function CRC32Buffer(AData: PByte; ASize: int64): Cardinal;
function CRC32File(const AFilename: string): Cardinal;
function CRC32(const AText: string): Cardinal;

function CRC64Stream(AStream: TStream; ABufferSize: Cardinal = 1024): UInt64;
function CRC64Buffer(const AData: PByte; const ASize: int64): UInt64;
function CRC64File(const AFilename: string): UInt64;
function CRC64(const AText: string): UInt64;

implementation

const
  CRC16Table: array [Byte] of Word = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241,
    $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
    $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40,
    $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
    $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
    $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
    $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641,
    $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
    $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240,
    $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
    $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
    $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41,
    $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
    $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
    $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
    $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240,
    $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
    $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41,
    $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
    $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41,
    $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640,
    $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
    $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
    $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
    $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40,
    $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
    $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40,
    $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
    $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641,
    $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040
  );

  CRC32Table: array [Byte] of Cardinal = (
    $00000000,$77073096,$EE0E612C,$990951BA,$076DC419,$706AF48F,$E963A535,$9E6495A3,
    $0EDB8832,$79DCB8A4,$E0D5E91E,$97D2D988,$09B64C2B,$7EB17CBD,$E7B82D07,$90BF1D91,
    $1DB71064,$6AB020F2,$F3B97148,$84BE41DE,$1ADAD47D,$6DDDE4EB,$F4D4B551,$83D385C7,
    $136C9856,$646BA8C0,$FD62F97A,$8A65C9EC,$14015C4F,$63066CD9,$FA0F3D63,$8D080DF5,
    $3B6E20C8,$4C69105E,$D56041E4,$A2677172,$3C03E4D1,$4B04D447,$D20D85FD,$A50AB56B,
    $35B5A8FA,$42B2986C,$DBBBC9D6,$ACBCF940,$32D86CE3,$45DF5C75,$DCD60DCF,$ABD13D59,
    $26D930AC,$51DE003A,$C8D75180,$BFD06116,$21B4F4B5,$56B3C423,$CFBA9599,$B8BDA50F,
    $2802B89E,$5F058808,$C60CD9B2,$B10BE924,$2F6F7C87,$58684C11,$C1611DAB,$B6662D3D,
    $76DC4190,$01DB7106,$98D220BC,$EFD5102A,$71B18589,$06B6B51F,$9FBFE4A5,$E8B8D433,
    $7807C9A2,$0F00F934,$9609A88E,$E10E9818,$7F6A0DBB,$086D3D2D,$91646C97,$E6635C01,
    $6B6B51F4,$1C6C6162,$856530D8,$F262004E,$6C0695ED,$1B01A57B,$8208F4C1,$F50FC457,
    $65B0D9C6,$12B7E950,$8BBEB8EA,$FCB9887C,$62DD1DDF,$15DA2D49,$8CD37CF3,$FBD44C65,
    $4DB26158,$3AB551CE,$A3BC0074,$D4BB30E2,$4ADFA541,$3DD895D7,$A4D1C46D,$D3D6F4FB,
    $4369E96A,$346ED9FC,$AD678846,$DA60B8D0,$44042D73,$33031DE5,$AA0A4C5F,$DD0D7CC9,
    $5005713C,$270241AA,$BE0B1010,$C90C2086,$5768B525,$206F85B3,$B966D409,$CE61E49F,
    $5EDEF90E,$29D9C998,$B0D09822,$C7D7A8B4,$59B33D17,$2EB40D81,$B7BD5C3B,$C0BA6CAD,
    $EDB88320,$9ABFB3B6,$03B6E20C,$74B1D29A,$EAD54739,$9DD277AF,$04DB2615,$73DC1683,
    $E3630B12,$94643B84,$0D6D6A3E,$7A6A5AA8,$E40ECF0B,$9309FF9D,$0A00AE27,$7D079EB1,
    $F00F9344,$8708A3D2,$1E01F268,$6906C2FE,$F762575D,$806567CB,$196C3671,$6E6B06E7,
    $FED41B76,$89D32BE0,$10DA7A5A,$67DD4ACC,$F9B9DF6F,$8EBEEFF9,$17B7BE43,$60B08ED5,
    $D6D6A3E8,$A1D1937E,$38D8C2C4,$4FDFF252,$D1BB67F1,$A6BC5767,$3FB506DD,$48B2364B,
    $D80D2BDA,$AF0A1B4C,$36034AF6,$41047A60,$DF60EFC3,$A867DF55,$316E8EEF,$4669BE79,
    $CB61B38C,$BC66831A,$256FD2A0,$5268E236,$CC0C7795,$BB0B4703,$220216B9,$5505262F,
    $C5BA3BBE,$B2BD0B28,$2BB45A92,$5CB36A04,$C2D7FFA7,$B5D0CF31,$2CD99E8B,$5BDEAE1D,
    $9B64C2B0,$EC63F226,$756AA39C,$026D930A,$9C0906A9,$EB0E363F,$72076785,$05005713,
    $95BF4A82,$E2B87A14,$7BB12BAE,$0CB61B38,$92D28E9B,$E5D5BE0D,$7CDCEFB7,$0BDBDF21,
    $86D3D2D4,$F1D4E242,$68DDB3F8,$1FDA836E,$81BE16CD,$F6B9265B,$6FB077E1,$18B74777,
    $88085AE6,$FF0F6A70,$66063BCA,$11010B5C,$8F659EFF,$F862AE69,$616BFFD3,$166CCF45,
    $A00AE278,$D70DD2EE,$4E048354,$3903B3C2,$A7672661,$D06016F7,$4969474D,$3E6E77DB,
    $AED16A4A,$D9D65ADC,$40DF0B66,$37D83BF0,$A9BCAE53,$DEBB9EC5,$47B2CF7F,$30B5FFE9,
    $BDBDF21C,$CABAC28A,$53B39330,$24B4A3A6,$BAD03605,$CDD70693,$54DE5729,$23D967BF,
    $B3667A2E,$C4614AB8,$5D681B02,$2A6F2B94,$B40BBE37,$C30C8EA1,$5A05DF1B,$2D02EF8D
  );

  CRC64Table: array [Byte] of UInt64 = (
    $0000000000000000, $42f0e1eba9ea3693, $85e1c3d753d46d26,
		$c711223cfa3e5bb5, $493366450e42ecdf, $0bc387aea7a8da4c,
		$ccd2a5925d9681f9, $8e224479f47cb76a, $9266cc8a1c85d9be,
		$d0962d61b56fef2d, $17870f5d4f51b498, $5577eeb6e6bb820b,
		$db55aacf12c73561, $99a54b24bb2d03f2, $5eb4691841135847,
		$1c4488f3e8f96ed4, $663d78ff90e185ef, $24cd9914390bb37c,
		$e3dcbb28c335e8c9, $a12c5ac36adfde5a, $2f0e1eba9ea36930,
		$6dfeff5137495fa3, $aaefdd6dcd770416, $e81f3c86649d3285,
		$f45bb4758c645c51, $b6ab559e258e6ac2, $71ba77a2dfb03177,
		$334a9649765a07e4, $bd68d2308226b08e, $ff9833db2bcc861d,
		$388911e7d1f2dda8, $7a79f00c7818eb3b, $cc7af1ff21c30bde,
		$8e8a101488293d4d, $499b3228721766f8, $0b6bd3c3dbfd506b,
		$854997ba2f81e701, $c7b97651866bd192, $00a8546d7c558a27,
		$4258b586d5bfbcb4, $5e1c3d753d46d260, $1cecdc9e94ace4f3,
		$dbfdfea26e92bf46, $990d1f49c77889d5, $172f5b3033043ebf,
		$55dfbadb9aee082c, $92ce98e760d05399, $d03e790cc93a650a,
		$aa478900b1228e31, $e8b768eb18c8b8a2, $2fa64ad7e2f6e317,
		$6d56ab3c4b1cd584, $e374ef45bf6062ee, $a1840eae168a547d,
		$66952c92ecb40fc8, $2465cd79455e395b, $3821458aada7578f,
		$7ad1a461044d611c, $bdc0865dfe733aa9, $ff3067b657990c3a,
		$711223cfa3e5bb50, $33e2c2240a0f8dc3, $f4f3e018f031d676,
		$b60301f359dbe0e5, $da050215ea6c212f, $98f5e3fe438617bc,
		$5fe4c1c2b9b84c09, $1d14202910527a9a, $93366450e42ecdf0,
		$d1c685bb4dc4fb63, $16d7a787b7faa0d6, $5427466c1e109645,
		$4863ce9ff6e9f891, $0a932f745f03ce02, $cd820d48a53d95b7,
		$8f72eca30cd7a324, $0150a8daf8ab144e, $43a04931514122dd,
		$84b16b0dab7f7968, $c6418ae602954ffb, $bc387aea7a8da4c0,
		$fec89b01d3679253, $39d9b93d2959c9e6, $7b2958d680b3ff75,
		$f50b1caf74cf481f, $b7fbfd44dd257e8c, $70eadf78271b2539,
		$321a3e938ef113aa, $2e5eb66066087d7e, $6cae578bcfe24bed,
		$abbf75b735dc1058, $e94f945c9c3626cb, $676dd025684a91a1,
		$259d31cec1a0a732, $e28c13f23b9efc87, $a07cf2199274ca14,
		$167ff3eacbaf2af1, $548f120162451c62, $939e303d987b47d7,
		$d16ed1d631917144, $5f4c95afc5edc62e, $1dbc74446c07f0bd,
		$daad56789639ab08, $985db7933fd39d9b, $84193f60d72af34f,
		$c6e9de8b7ec0c5dc, $01f8fcb784fe9e69, $43081d5c2d14a8fa,
		$cd2a5925d9681f90, $8fdab8ce70822903, $48cb9af28abc72b6,
		$0a3b7b1923564425, $70428b155b4eaf1e, $32b26afef2a4998d,
		$f5a348c2089ac238, $b753a929a170f4ab, $3971ed50550c43c1,
		$7b810cbbfce67552, $bc902e8706d82ee7, $fe60cf6caf321874,
		$e224479f47cb76a0, $a0d4a674ee214033, $67c58448141f1b86,
		$253565a3bdf52d15, $ab1721da49899a7f, $e9e7c031e063acec,
		$2ef6e20d1a5df759, $6c0603e6b3b7c1ca, $f6fae5c07d3274cd,
		$b40a042bd4d8425e, $731b26172ee619eb, $31ebc7fc870c2f78,
		$bfc9838573709812, $fd39626eda9aae81, $3a28405220a4f534,
		$78d8a1b9894ec3a7, $649c294a61b7ad73, $266cc8a1c85d9be0,
		$e17dea9d3263c055, $a38d0b769b89f6c6, $2daf4f0f6ff541ac,
		$6f5faee4c61f773f, $a84e8cd83c212c8a, $eabe6d3395cb1a19,
		$90c79d3fedd3f122, $d2377cd44439c7b1, $15265ee8be079c04,
		$57d6bf0317edaa97, $d9f4fb7ae3911dfd, $9b041a914a7b2b6e,
		$5c1538adb04570db, $1ee5d94619af4648, $02a151b5f156289c,
		$4051b05e58bc1e0f, $87409262a28245ba, $c5b073890b687329,
		$4b9237f0ff14c443, $0962d61b56fef2d0, $ce73f427acc0a965,
		$8c8315cc052a9ff6, $3a80143f5cf17f13, $7870f5d4f51b4980,
		$bf61d7e80f251235, $fd913603a6cf24a6, $73b3727a52b393cc,
		$31439391fb59a55f, $f652b1ad0167feea, $b4a25046a88dc879,
		$a8e6d8b54074a6ad, $ea16395ee99e903e, $2d071b6213a0cb8b,
		$6ff7fa89ba4afd18, $e1d5bef04e364a72, $a3255f1be7dc7ce1,
		$64347d271de22754, $26c49cccb40811c7, $5cbd6cc0cc10fafc,
		$1e4d8d2b65facc6f, $d95caf179fc497da, $9bac4efc362ea149,
		$158e0a85c2521623, $577eeb6e6bb820b0, $906fc95291867b05,
		$d29f28b9386c4d96, $cedba04ad0952342, $8c2b41a1797f15d1,
		$4b3a639d83414e64, $09ca82762aab78f7, $87e8c60fded7cf9d,
		$c51827e4773df90e, $020905d88d03a2bb, $40f9e43324e99428,
		$2cffe7d5975e55e2, $6e0f063e3eb46371, $a91e2402c48a38c4,
		$ebeec5e96d600e57, $65cc8190991cb93d, $273c607b30f68fae,
		$e02d4247cac8d41b, $a2dda3ac6322e288, $be992b5f8bdb8c5c,
		$fc69cab42231bacf, $3b78e888d80fe17a, $7988096371e5d7e9,
		$f7aa4d1a85996083, $b55aacf12c735610, $724b8ecdd64d0da5,
		$30bb6f267fa73b36, $4ac29f2a07bfd00d, $08327ec1ae55e69e,
		$cf235cfd546bbd2b, $8dd3bd16fd818bb8, $03f1f96f09fd3cd2,
		$41011884a0170a41, $86103ab85a2951f4, $c4e0db53f3c36767,
		$d8a453a01b3a09b3, $9a54b24bb2d03f20, $5d45907748ee6495,
		$1fb5719ce1045206, $919735e51578e56c, $d367d40ebc92d3ff,
		$1476f63246ac884a, $568617d9ef46bed9, $e085162ab69d5e3c,
		$a275f7c11f7768af, $6564d5fde549331a, $279434164ca30589,
		$a9b6706fb8dfb2e3, $eb46918411358470, $2c57b3b8eb0bdfc5,
		$6ea7525342e1e956, $72e3daa0aa188782, $30133b4b03f2b111,
		$f7021977f9cceaa4, $b5f2f89c5026dc37, $3bd0bce5a45a6b5d,
		$79205d0e0db05dce, $be317f32f78e067b, $fcc19ed95e6430e8,
		$86b86ed5267cdbd3, $c4488f3e8f96ed40, $0359ad0275a8b6f5,
		$41a94ce9dc428066, $cf8b0890283e370c, $8d7be97b81d4019f,
		$4a6acb477bea5a2a, $089a2aacd2006cb9, $14dea25f3af9026d,
		$562e43b4931334fe, $913f6188692d6f4b, $d3cf8063c0c759d8,
		$5dedc41a34bbeeb2, $1f1d25f19d51d821, $d80c07cd676f8394,
		$9afce626ce85b507
  );

function CRC8(AText: string): Byte;
var
  x,y: Integer;
  b: Byte;
begin
  Result:=0;
  for x:=1 to Length(AText) do begin
    b:=Ord(AText[x]);
    for y:=0 to 7 do begin
      if ((b xor Result) and 1) = 1 then
        Result:=((Result xor $18) shr 1) or $80
      else
        Result:=Result shr 1;
      b:=b shr 1;
    end;
  end;
end;

function CRC16(const AText: string): Word;
var
  b: Byte;
  i: Integer;
begin
  Result:=$FFFF;
  for i:=1 to Length(AText) do begin
    b:=Ord(AText[i]) xor Result;
    Result:=Result shr 8 xor CRC16Table[b];
  end;
end;

function CRC16Buffer(AData: PByte; ASize: int64): Word;
var
  b: byte;
  d: PByte;
  i: int64;
begin
  Result:=$FFFF;
  d:=AData;
  i:=0;
  repeat
    b:=d^ xor Result;
    Result:=Result shr 8 xor CRC16Table[b];
    Inc(d);
    inc(i);
  until i>=ASize;
end;

function CRC32Stream(AStream: TStream; ABufferSize: Cardinal = 1024): Cardinal;
var
  n: integer;
  Buffer,b: PByte;
  i: Cardinal;
begin
  AStream.Position:=0;
  Result:=$ffffffff;
  Buffer:=Allocmem(ABufferSize);
  try
    repeat
      n:=AStream.Read(Buffer^,ABufferSize);
      b:=Buffer;
      for i:=1 to n do begin
        Result:=(Result shr 8) xor CRC32Table[b^ xor (Result and $000000FF)];
        Inc(b);
      end;
    until n=0;
  finally
    AStream.Position:=0;
    Freemem(Buffer);
  end;
  Result:=not Result;
end;

function CRC32Buffer(AData: PByte; ASize: int64): Cardinal;
var
  b: PByte;
  i: int64;
begin
  Result:=$ffffffff;
  b:=AData;
  i:=0;
  repeat
    Result:=(Result shr 8) xor CRC32Table[b^ xor (Result and $000000FF)];
    Inc(b);
    inc(i);
  until i>=ASize;
  Result:=not Result;
end;

function CRC32File(const AFilename: string): Cardinal;
var
  mf: TMappedFile;
begin
  Result:=0;
  mf:=TMappedFile.Create(AFilename);
  try
    if Assigned(mf.Content) then
      Result:=CRC32Buffer(mf.Content,mf.Size);
  finally
    mf.Free;
  end;
end;

function CRC32(const AText: string): Cardinal;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create(AText);
  try
    Result:=CRC32Stream(ss);
  finally
    ss.Free;
  end;
end;

function CRC64Buffer(const AData: PByte; const ASize: int64): UInt64;
var
  b: PByte;
  i: int64;
begin
  Result:=$FFFFFFFFFFFFFFFF;
  b:=AData;
  i:=0;
  repeat
    Result:=CRC64Table[(Result shr 56) xor b^] xor (Result shl 8);
    Inc(b);
    inc(i);
  until i>=ASize;
  Result:=not Result;
end;

function CRC64Stream(AStream: TStream; ABufferSize: Cardinal = 1024): UInt64;
var
  n: int64;
  Buffer,b: PByte;
  i: int64;
begin
  AStream.Position:=0;
  Result:=$FFFFFFFFFFFFFFFF;
  Buffer:=Allocmem(ABufferSize);
  try
    repeat
      n:=AStream.Read(Buffer^,ABufferSize);
      b:=Buffer;
      i:=0;
      while i<n do begin
        Result:=CRC64Table[(Result shr 56) xor b^] xor (Result shl 8);
        Inc(b);
        inc(i);
      end;
    until n=0;
  finally
    AStream.Position:=0;
    Freemem(Buffer);
  end;
  Result:=not Result;
end;

function CRC64File(const AFilename: string): UInt64;
var
  mf: TMappedFile;
begin
  Result:=0;
  mf:=TMappedFile.Create(AFilename);
  try
    if Assigned(mf.Content) then
      Result:=CRC64Buffer(mf.Content,mf.Size);
  finally
    mf.Free;
  end;
end;

function CRC64(const AText: string): UInt64;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create(AText);
  try
    Result:=CRC64Stream(ss,ss.Size);
  finally
    ss.Free;
  end;
end;

end.

