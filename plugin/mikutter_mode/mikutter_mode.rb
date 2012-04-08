# -*- coding: utf-8 -*-
require 'dbus'

Plugin.create(:mikutter_mode) do
  SERVICE_NAME = "org.mikutter.dynamic"

  bus = DBus::SessionBus.instance
  service = bus.request_service(SERVICE_NAME)

  class Server < DBus::Object
    dbus_interface "org.mikutter.eval" do
      dbus_method :ruby, "in code:s, out result:s" do |code|
        begin
          r = eval code
          [r.to_s]
        rescue Exception => e
          [e.to_s] end end end end

  exported_obj = Server.new("/org/mikutter/MyInstance")
  service.export(exported_obj)

  Thread.new do
    main = DBus::Main.new
    main << bus
    main.run
  end
end
