# Erlang 编程

[README.md](README.md)

[简介](introduction.md)


[入门](getting_started.md)


---
+ [为何是 Erlang？](part-i.md)
    - [并发介绍](part-i/Ch01_introducing-concurrency.md)
    - [Erlang 旋风之旅](part-i/Ch02_a-whirlwind-tour-of-erlang.md)

---
+ [顺序编程](part-ii.md)
    - [基本概念](part-ii/Ch03-basic_concepts.md)
    - [模组与函数](part-ii/Ch04-modules_and_functions.md)
    - [记录与映射](part-ii/Ch05-records_and_maps.md)
    - [顺序程序中的错误处理](part-ii/Ch06-error_handling_in_sequential_programs.md)
    - [二进制值与位语法](part-ii/Ch07-binaries_and_the_bit_syntax.md)
    - [顺序 Erlang 的其余部分](part-ii/Ch08-the_rest_of_sequential_erlang.md)
    - [类型](part-ii/Ch09-types.md)
    - [编译与运行咱们的程序](part-ii/Ch10-compiling_and_running_your_program.md)


---
+ [并发与分布式程序](part-iii.md)
    - [真实世界的并发](part-iii/Ch11-real-world_concurrency.md)
    - [并发编程](part-iii/Ch12-concurrent_programming.md)
    - [并发程序中的错误](part-iii/Ch13-errors_in_concurrent_programs.md)
    - [分布式编程](part-iii/Ch14-distributed_programming.md)



---
+ [编程库与框架](part-iv.md)
    - [接口技术](part-iv/Ch15-interfacing_techniques.md)
    - [文件编程](part-iv/Ch16-programming_with_files.md)
    - [套接字编程](part-iv/Ch17-programming_with_sockets.md)
    - [使用 websockets 和 Erlang 浏览网页](part-iv/Ch18-browsing_with_websockets_and_erlang.md)
    - [使用 ETS 和 DETS 存储数据](part-iv/Ch19-storing_data_with_ets_and_dets.md)
    - [Mnesia：Erlang 的数据库](part-iv/Ch20-mnesia_the_erlang_database.md)
    - [性能分析、调试与追踪](part-iv/Ch21-profiling_debugging_and_tracing.md)
    - [引入 OTP](part-iv/Ch22-introducing_otp.md)
    - [使用 OTP 构造系统](part-iv/Ch23-making_a_system_with_otp.md)



---
+ [构建应用](part-v.md)
    - [编程习语](part-v/Ch24-programming_idioms.md)
    - [第三方程序](part-v/Ch25-third_party_programs.md)
    - [多核心 CPU 编程](part-v/Ch26-programming_multicore_CPUs.md)
    - [福尔摩斯的最后一案](part-v/Ch27-sherlock-s_last_case.md)

---

+ [附录 1，OTP 模板](appendix/ap01-otp_templates.md)
    - [通用服务器模板](appendix/ap01/gen_server_tmpl.md)
    - [监控器模板](appendix/ap01/supervisor_tmpl.md)
    - [应用模板](appendix/ap01/app_tmpl.md)


+ [附录 2，一个套接字应用](appendix/ap02-a_socket_application.md)
    - [一个示例](appendix/ap02/an_example.md)
    - [`lib_chan` 工作原理](appendix/ap02/lib_chan-works.md)
    - [`lib_chan` 的代码](appendix/ap02/lib_chan-code.md)


+ [附录 3，一种简单执行环境](appendix/ap03-a_simple_execution_environment.md)
    - [Erlang 如何启动](appendix/ap03/erlang-starts.md)
    - [在 SEE 下运行一些测试程序](appendix/ap03/running_tests.md)
    - [SEE 的 API](appendix/ap03/SEE_API.md)
    - [SEE 实现细节](appendix/ap03/impl_details.md)
