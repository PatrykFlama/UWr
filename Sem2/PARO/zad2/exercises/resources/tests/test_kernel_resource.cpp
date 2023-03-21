#include <resources/kernel.h>
#include <resources/kernel_resource.h>

#include <gmock/gmock.h>

using namespace ::testing;

struct KernelConfigMock : KernelConfig
{
    MOCK_METHOD(int, get_next_resource_id, (), (noexcept));
    MOCK_METHOD(bool, is_allocation_successful, (), (noexcept));
    MOCK_METHOD(bool, is_operation_successful, (), (noexcept));
};

struct KernelPolicyMock : KernelPolicy
{
    MOCK_METHOD(void, handle_leaked_resource, (RESOURCE*), (const));
};

struct KernelResourceTest : Test
{
    KernelResourceTest()
        : kernel_config(*new KernelConfigMock),
          kernel_policy(*new KernelPolicyMock)
    {
        std::unique_ptr<KernelConfig> kernel_config_ptr{&kernel_config};
        std::unique_ptr<KernelPolicy> kernel_policy_ptr{&kernel_policy};

        reconfigure_kernel(std::move(kernel_config_ptr),
                           std::move(kernel_policy_ptr));
    }

    ~KernelResourceTest() override { finalize_kernel(); }

    KernelConfigMock& kernel_config;
    KernelPolicyMock& kernel_policy;
};

TEST_F(KernelResourceTest, test_allocate)
{
    EXPECT_CALL(kernel_config, get_next_resource_id()).WillOnce(Return(0x10));
    EXPECT_CALL(kernel_config, is_allocation_successful())
        .WillOnce(Return(true));

    auto resource = allocate_resource();

    ASSERT_EQ((long)resource, 0x10);

    ASSERT_NO_THROW(free_resource(resource));
    EXPECT_CALL(kernel_policy, handle_leaked_resource(_)).Times(0);
}

TEST_F(KernelResourceTest, test_allocation_fail_nullptr)
{
    EXPECT_CALL(kernel_config, get_next_resource_id()).WillOnce(Return(0x10));
    EXPECT_CALL(kernel_config, is_allocation_successful())
        .WillOnce(Return(false));

    auto resource = allocate_resource();

    ASSERT_EQ(resource, nullptr);
}

TEST_F(KernelResourceTest, test_allocation_fail_exception)
{
    EXPECT_CALL(kernel_config, get_next_resource_id())
        .Times(2)
        .WillRepeatedly(Return(0x11));
    EXPECT_CALL(kernel_config, is_allocation_successful())
        .Times(2)
        .WillRepeatedly(Return(true));

    auto resource = allocate_resource();
    ASSERT_EQ((long)resource, 0x11);
    ASSERT_THROW(allocate_resource(), std::runtime_error);

    ASSERT_NO_THROW(free_resource(resource));
    EXPECT_CALL(kernel_policy, handle_leaked_resource(_)).Times(0);
}

TEST_F(KernelResourceTest, test_deallocate_null)
{
    ASSERT_DEATH(free_resource(nullptr), "terminate");
}

TEST_F(KernelResourceTest, test_deallocate_invalid)
{
    ASSERT_DEATH(free_resource((RESOURCE*)0x1234), "terminate");
}

TEST_F(KernelResourceTest, test_raii_wrapper)
{
    EXPECT_CALL(kernel_config, get_next_resource_id()).WillOnce(Return(0x10));
    EXPECT_CALL(kernel_config, is_allocation_successful())
        .WillOnce(Return(true));

    std::unique_ptr<RESOURCE, decltype(&free_resource)> raii_handler(
        allocate_resource(), &free_resource);

    EXPECT_CALL(kernel_config, is_operation_successful())
        .WillOnce(Return(true));
    ASSERT_NO_THROW(use_resource(raii_handler.get()));
}
