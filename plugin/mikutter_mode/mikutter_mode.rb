# -*- coding: utf-8 -*-
require 'dbus'

Plugin.create(:mikutter_mode) do
  SERVICE_NAME = "org.mikutter.dynamic"

  bus = DBus::SessionBus.instance
  service = bus.request_service(SERVICE_NAME)

  class Server < DBus::Object
    class << self
      attr_accessor :main end

    dbus_interface "org.mikutter.eval" do
      dbus_method :ruby, "in param:a(sv), out result:s" do |_param|
        param = {}
        _param.each{ |pair|
          param[pair[0]] = pair[1] }
        code, file = param['code'], param['file']
        begin
          notice "ruby code execute: \n#{file || code}"
          r = Server.main.instance_eval(code, file || "mikutter-mode onthefly executer")
          notice "returns: \n#{r.inspect}"
          [r.to_s]
        rescue Exception => e
          notice "exception:"
          notice e
          [e.to_s] end end end end

  exported_obj = Server.new("/org/mikutter/MyInstance")
  service.export(exported_obj)

  Thread.new{
    loop {
      begin
        main = DBus::Main.new
        main << bus
        main.run
      rescue => e
        error e
      ensure
        sleep 1
      end
    }
  }
end
Plugin.create(:mikutter_mode).instance_eval{ Server }.main = self
