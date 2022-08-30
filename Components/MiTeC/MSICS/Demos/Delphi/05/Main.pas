unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, MiTeC_WinIOCTL, StdCtrls, ComCtrls, ExtCtrls, ImgList, MSI_Storage,
  MSI_Disk, MSI_Common, MSI_DeviceMonitor, MiTeC_CfgMgrSetupApi;

type
  Twnd_dv_Main = class(TForm)
    Tree: TTreeView;
    bRefresh: TButton;
    bSave: TButton;
    bClose: TButton;
    sd: TSaveDialog;
    List: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    ilSystem: TImageList;
    cbxAuto: TCheckBox;
    procedure DeviceMonitorVolumeConnect(Sender: TObject; Drives: string;
      Remote: Boolean);
    procedure cmRefresh(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure bSaveClick(Sender: TObject);
  private
    Storage: TMiTeC_Storage;
    Disk: TMiTeC_Disk;
    DeviceMonitor: TMiTeC_DeviceMonitor;
    spid: TSPClassImageListData;
  public
  end;

var
  wnd_dv_Main: Twnd_dv_Main;

implementation

uses MiTeC_StrUtils, MiTeC_Routines, MiTeC_Storage;

{$R *.dfm}

procedure Twnd_dv_Main.cmRefresh(Sender: TObject);
var
  rd,c,i,j,ii: Integer;
  n,r: TTreeNode;
  pi: PInteger;
  s,d: string;
  g: TGUID;
begin
  d:='';
  Tree.OnChange:=nil;
  with Storage do
    try
      Screen.Cursor:=crHourGlass;
      RefreshData;
      Tree.Items.BeginUpdate;
      try
      Tree.Items.Clear;
      c:=PhysicalCount;
      for i:=0 to PhysicalCount-1 do
        with Physical[i] do begin
          New(pi);
          pi^:=i;
          if Size>0 then
            r:=Tree.Items.AddChildObject(nil,Trim(Format('%s (%d MB)',[Model,Size shr 20])),pi)
          else
            r:=Tree.Items.AddChildObject(nil,Trim(Format('%s',[Model])),pi);
          g:=GUID_DEVCLASS_DISKDRIVE;
          case DeviceType of
            FILE_DEVICE_CD_ROM,
            FILE_DEVICE_DVD: g:=GUID_DEVCLASS_CDROM;
            FILE_DEVICE_TAPE: g:=GUID_DEVCLASS_TAPEDRIVE;
            FILE_DEVICE_DISK: if Removable then
                                 g:=GUID_DEVCLASS_FDC;
          end;
          SetupDiGetClassImageIndex(spid,g,ii);
          r.ImageIndex:=ii;
          r.SelectedIndex:=r.ImageIndex;
          for j:=0 to LogicalCount-1 do
            with Logical[j] do
              if PhysicalIndex=i then begin
                d:=d+Copy(Drive,1,1);
                New(pi);
                pi^:=j;
                Disk.Drive:=Drive+':';
                if Disk.Capacity=0 then
                  n:=Tree.Items.AddChildObject(r,Format('%s:',[Drive]),pi)
                else begin
                  if not(DeviceType in [FILE_DEVICE_CD_ROM,FILE_DEVICE_DVD, FILE_DEVICE_TAPE,FILE_DEVICE_UNKNOWN]) and (Length(Layout)>0) and (LayoutIndex>-1) then
                    n:=Tree.Items.AddChildObject(r,Format('%s: (%s %s - %d MB)',[
                                     Drive,
                                     GetPartitionType(Layout[LayoutIndex].Number,Layout[LayoutIndex].Typ),
                                     FileSystem,//GetPartitionSystem(Layout[LayoutIndex].Typ),
                                     Layout[LayoutIndex].Length.QuadPart shr 20]),pi)
                  else
                    n:=Tree.Items.AddChildObject(r,Format('%s: (%s - %d MB)',[Drive,Disk.FileSystem,Disk.Capacity shr 20]),pi);
                end;
                g:=GUID_DEVCLASS_VOLUME;
                SetupDiGetClassImageIndex(spid,g,ii);
                n.ImageIndex:=ii;
                n.SelectedIndex:=n.ImageIndex;
              end;
      end;

      new(pi);
      pi^:=-1;
      r:=Tree.Items.AddChildObject(nil,'Network drives',pi);
      g:=GUID_DEVCLASS_DISKDRIVE;
      SetupDiGetClassImageIndex(spid,g,ii);
      r.ImageIndex:=ii;
      r.SelectedIndex:=r.ImageIndex;
      Disk.RefreshData;
      s:=Disk.AvailableDisks;
      with Disk do
        for i:=1 to Length(s) do begin
          Drive:=Format('%s:\',[Copy(s,i,1)]);
          if MediaType=dtRemote then begin
             d:=d+Copy(Drive,1,1);
             new(pi);
             pi^:=i;
             n:=Tree.Items.AddChildObject(r,Format('%s (%s)',[Drive,UNCPath]),pi);
             g:=GUID_DEVCLASS_VOLUME;
             SetupDiGetClassImageIndex(spid,g,ii);
             n.ImageIndex:=ii;
             n.SelectedIndex:=n.ImageIndex;
          end;
        end;
      if r.Count=0 then
        Tree.Items.Delete(r);

      new(pi);
      pi^:=-2;
      r:=Tree.Items.AddChildObject(nil,'Removable drives',pi);
      g:=GUID_DEVCLASS_DISKDRIVE;
      SetupDiGetClassImageIndex(spid,g,ii);
      r.ImageIndex:=ii;
      r.SelectedIndex:=r.ImageIndex;
      with Disk do
        for i:=1 to Length(s) do begin
          Drive:=Format('%s:\',[Copy(s,i,1)]);
          if (Pos(Copy(s,i,1),d)=0) and (MediaType=dtRemovable) then begin
            d:=d+Copy(Drive,1,1);
            new(pi);
            pi^:=i;
            n:=Tree.Items.AddChildObject(r,Format('%s',[Drive]),pi);
            g:=GUID_DEVCLASS_VOLUME;
            SetupDiGetClassImageIndex(spid,g,ii);
            n.ImageIndex:=ii;
            n.SelectedIndex:=n.ImageIndex;
            Inc(rd);
          end;
        end;
      if r.Count=0 then
        Tree.Items.Delete(r);

      new(pi);
      pi^:=-2;
      r:=Tree.Items.AddChildObject(nil,'Other drives',pi);
      g:=GUID_DEVCLASS_DISKDRIVE;
      SetupDiGetClassImageIndex(spid,g,ii);
      r.ImageIndex:=ii;
      r.SelectedIndex:=r.ImageIndex;
      with Disk do
        for i:=1 to Length(s) do begin
          if Pos(Copy(s,i,1),d)=0 then begin
            Drive:=Format('%s:\',[Copy(s,i,1)]);
            new(pi);
            pi^:=i;
            n:=Tree.Items.AddChildObject(r,Format('%s',[Drive]),pi);
            g:=GUID_DEVCLASS_VOLUME;
            SetupDiGetClassImageIndex(spid,g,ii);
            n.ImageIndex:=ii;
            n.SelectedIndex:=n.ImageIndex;
          end;
        end;
      if r.Count=0 then
        Tree.Items.Delete(r);

      Tree.FullExpand;
      finally
        Tree.Items.EndUpdate;
      end;
    finally
      Screen.Cursor:=crDefault;
      Caption:=Format('Storage Devices (%d physical, %d logical)',[PhysicalCount+rd, Length(s)]);
    end;
  Tree.Items[0].MakeVisible;
  Tree.OnChange:=TreeChange;
end;

procedure Twnd_dv_Main.DeviceMonitorVolumeConnect(Sender: TObject; Drives: string;
  Remote: Boolean);
begin
  if cbxAuto.Checked then
    cmRefresh(nil);
end;

procedure Twnd_dv_Main.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure Twnd_dv_Main.FormCreate(Sender: TObject);
begin
  spid.cbSize:=sizeof(spid);
  SetupDiGetClassImageList(spid);
  ilSystem.Handle:=spid.ImageList;
  Storage:=TMiTeC_Storage.Create(Self);
  Disk:=TMiTeC_Disk.Create(Self);
  DeviceMonitor:=TMiTeC_DeviceMonitor.Create(Self);
  with DeviceMonitor do begin
    Active:=True;
    CatchBluetooth:=False;
    OnVolumeConnect:=DeviceMonitorVolumeConnect;
    OnVolumeDisconnect:=DeviceMonitorVolumeConnect;
  end;
  cmRefresh(nil);
end;

procedure Twnd_dv_Main.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    Dispose(PInteger(Node.Data));
end;

procedure Twnd_dv_Main.TreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Assigned(Node) then
    if (Node.Level=0) then begin
      Sender.Canvas.Font.Style:=[fsBold];
      if (cdsSelected in State) and not Sender.Focused then
        Sender.Canvas.Font.Color:=clWindowText;
    end;
end;

procedure Twnd_dv_Main.TreeChange(Sender: TObject; Node: TTreeNode);
var
  s: string;
begin
  List.Items.BeginUpdate;
  List.Columns.BeginUpdate;
  try
    List.Items.Clear;
    if Assigned(Node) and Assigned(Node.Data) then begin
      if PInteger(Node.Data)^<0 then
        Exit;
      if Node.Level=0 then with Storage.Physical[PInteger(Node.Data)^] do begin
        with List.Items.Add do begin
          Caption:='Serial number';
          SubItems.Add(SerialNumber);
        end;
        with List.Items.Add do begin
          Caption:='Revision';
          SubItems.Add(Revision);
        end;
        with List.Items.Add do begin
          Caption:='Model';
          SubItems.Add(Model);
        end;
        with List.Items.Add do begin
          Caption:='Bus Type';
          SubItems.Add(GetStorageBusTypeStr(BusType));
        end;
        s:='';
        case DeviceType of
          FILE_DEVICE_CD_ROM: s:='CD-ROM';
          FILE_DEVICE_DVD: s:='DVD';
          FILE_DEVICE_MASS_STORAGE: s:='Mass Storage';
          FILE_DEVICE_TAPE: s:='Tape';
          FILE_DEVICE_DISK: begin
            s:='Drive';
            if SSD then
              s:='Solid State Drive';
          end;
        end;
        if s<>'' then
          with List.Items.Add do begin
            Caption:='Device Type';
            SubItems.Add(s);
          end;
        with List.Items.Add do begin
          Caption:='Media Type';
          SubItems.Add(GetDeviceMediaTypeStr(MediaType));
        end;
        if (DeviceType=FILE_DEVICE_CD_ROM) or (DeviceType=FILE_DEVICE_DVD) then begin
          s:='';
          if Read_CDRW then
            s:=s+'CD-R,';
          if Read_CDR then
            s:=s+'CD-RW,';
          if Read_DVDROM then
            s:=s+'DVD-ROM,';
          if Read_DVDR then
            s:=s+'DVD-R,';
          if Read_DVDRAM then
            s:=s+'DVD-RAM,';
          SetLength(s,Length(s)-1);
          with List.Items.Add do begin
            Caption:='Read Caps';
            SubItems.Add(s);
          end;
          s:='';
          if Write_CDRW then
            s:=s+'CD-R,';
          if Write_CDR then
            s:=s+'CD-RW,';
          if Write_DVDR then
            s:=s+'DVD-R,';
          if Write_DVDRAM then
            s:=s+'DVD-RAM,';
          SetLength(s,Length(s)-1);
          with List.Items.Add do begin
            Caption:='Write Caps';
            SubItems.Add(s);
          end;
        end;
        if HaId>=0 then
          with List.Items.Add do begin
            Caption:='SCSI adapter';
            SubItems.Add(IntToStr(HaId));
          end;
        if PathId>=0 then
          with List.Items.Add do begin
            Caption:='Bus';
            SubItems.Add(IntToStr(PathId));
          end;
        if Target>=0 then
          with List.Items.Add do begin
            Caption:='Target device';
            SubItems.Add(IntToStr(Target));
          end;
        if Lun>=0 then
          with List.Items.Add do begin
            Caption:='Logical unit number';
            SubItems.Add(IntToStr(Lun));
          end;
        //end;
        if Temperature>0 then
          with List.Items.Add do begin
            Caption:='Temperature';
            SubItems.Add(Format('%d°C',[Temperature]));
          end;
        if Size>0 then begin
          with List.Items.Add do begin
            Caption:='Capacity';
            SubItems.Add(Format('%d MB',[Size shr 20]));
          end;
          with List.Items.Add do begin
            Caption:='Cyls';
            SubItems.Add(Format('%d',[Geometry.Cylinders.QuadPart]));
          end;
          with List.Items.Add do begin
            Caption:='Heads';
            SubItems.Add(Format('%d',[Geometry.TracksPerCylinder]));
          end;
          with List.Items.Add do begin
            Caption:='Sectors per track';
            SubItems.Add(Format('%d',[Geometry.SectorsPerTrack]));
          end;
          with List.Items.Add do begin
            Caption:='Bytes per sector';
            SubItems.Add(Format('%d',[Geometry.BytesPerSector]));
          end;
          if (IdentifyDeviceData.Max48BitLBA[0]=0) then
            with List.Items.Add do begin
              Caption:='Physical sectors';
              if (Geometry.BytesPerSector>0) then
                SubItems.Add(Format('%d',[LengthInBytes div Geometry.BytesPerSector]))
              else
               SubItems.Add(Format('%d',[Geometry.Cylinders.QuadPart*Geometry.TracksPerCylinder*Geometry.SectorsPerTrack]));
            end
          else begin
            with List.Items.Add do begin
              Caption:='Number of sectors';
              SubItems.Add(Format('%d',[IdentifyDeviceData.CurrentSectorCapacity]));
            end;
            with List.Items.Add do begin
              Caption:='Total 32-bit LBA sectors';
              SubItems.Add(Format('%d',[IdentifyDeviceData.UserAddressableSectors]));
            end;
            with List.Items.Add do begin
              Caption:='Total 48-bit LBA sectors';
              SubItems.Add(Format('%d',[IdentifyDeviceData.Max48BitLBA[0]]));
            end;
          end;
        end;
        if IdentifyDeviceData.MajorRevision>0 then
          with List.Items.Add do begin
            Caption:='ATA Major version';
            SubItems.Add(GetATAMajorVersion(IdentifyDeviceData.MajorRevision));
          end;
        if IdentifyDeviceData.MinorRevision>0 then
          with List.Items.Add do begin
            Caption:='ATA Minor version';
            SubItems.Add(GetATAMinorVersion(IdentifyDeviceData.MinorRevision));
          end;
        if IdentifyDeviceData.ReservedWord220[2]>0 then
          with List.Items.Add do begin
            Caption:='ATA Transport version';
            SubItems.Add(GetATATransportVersion(IdentifyDeviceData.ReservedWord220[2]));
          end;
        if ECCCode<>0 then
          with List.Items.Add do begin
            Caption:='ECC';
            SubItems.Add(Format('%d',[ECCCode]));
          end;
        if CtlBufSize<>0 then
          with List.Items.Add do begin
            Caption:='Cache Buffer size';
            SubItems.Add(Format('%d KB',[CtlBufSize shr 10]));
          end;
        with List.Items.Add do begin
          Caption:='S.M.A.R.T.';
          if SMARTSupport then begin
            if SMARTActive then
              SubItems.Add('Supported and active')
            else
              SubItems.Add('Supported and NOT active');
          end else
            SubItems.Add('NOT supported');
        end;
      end else begin
        if PInteger(Node.Parent.Data)^<0 then
          Disk.Drive:=Copy(Disk.AvailableDisks,PInteger(Node.Data)^,1)+':'
        else
          Disk.Drive:=Storage.Logical[PInteger(Node.Data)^].Drive+':';
        with List.Items.Add do begin
          Caption:='Volume label';
          SubItems.Add(Disk.VolumeLabel);
        end;
        with List.Items.Add do begin
          Caption:='Serial number';
          SubItems.Add(Disk.SerialNumber);
        end;
        with List.Items.Add do begin
          Caption:='Capacity';
          SubItems.Add(Format('%d MB',[Disk.Capacity shr 20]));
        end;
        with List.Items.Add do begin
          Caption:='Free';
          SubItems.Add(Format('%d MB',[Disk.FreeSpace shr 20]));
        end;
        with List.Items.Add do begin
          Caption:='File system';
          SubItems.Add(Disk.FileSystem);
        end;
        with Storage.Logical[PInteger(Node.Data)^] do
          if LayoutIndex>-1 then begin
            with List.Items.Add do begin
              Caption:='First sector';
              SubItems.Add(IntToStr(Layout[LayoutIndex].StartingOffset.QuadPart div Geometry.BytesPerSector));
            end;
            with List.Items.Add do begin
              Caption:='Last sector';
              SubItems.Add(IntToStr((Layout[LayoutIndex].StartingOffset.QuadPart+Layout[LayoutIndex].Length.QuadPart-1) div Geometry.BytesPerSector));
            end;
            with List.Items.Add do begin
              Caption:='Cluster size';
              SubItems.Add(Format('%d B',[ClusterSize]));
            end;
            with List.Items.Add do begin
              Caption:='Total sectors';
              SubItems.Add(IntToStr(Layout[LayoutIndex].Length.QuadPart div Geometry.BytesPerSector));
            end;
          end;
      end;
    end;
  finally
    List.Items.EndUpdate;
    List.Columns.EndUpdate;
  end;
end;

procedure Twnd_dv_Main.bSaveClick(Sender: TObject);
var
 h: Boolean;
begin
  h:=True;
  if sd.Execute then
    Storage.SaveToStorage(sd.FileName,h);
end;

end.
