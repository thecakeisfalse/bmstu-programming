
def memoize(name)
  @lookup ||= Hash.new { |h, k| h[k] = {} }
  f = singleton_class.instance_method(name)
  define_singleton_method name do |*args|
    @lookup[name][args] =
      @lookup[name].has_key?(args) ?
        @lookup[name][args] : f.bind(self).call(*args)
  end
end
