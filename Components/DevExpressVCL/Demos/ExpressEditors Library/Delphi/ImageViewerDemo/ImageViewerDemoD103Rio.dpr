program ImageViewerDemoD103Rio;

uses
  Forms,
  BaseForm in '..\BaseForm.pas',
  DemoUtils in '..\DemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas',
  ImageViewerDemoMain in 'ImageViewerDemoMain.pas' {ImageViewerDemoMainForm},
  ImageViewerDemoResizeImage in 'ImageViewerDemoResizeImage.pas' {ImageViewerDemoResizeImageForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.CreateForm(TImageViewerDemoMainForm, ImageViewerDemoMainForm);
  Application.CreateForm(TImageViewerDemoResizeImageForm, ImageViewerDemoResizeImageForm);
  Application.Run;
end.
